{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Keter.Proxy.Rewrite
  ( RewritePath (..)
  , MatchType (..)
  , rewrite
  , rewritePathParts
  , rewritePathRule
  , mergeQueries
  , checkRegexVars
  )
  where

import           Control.Applicative         ((<$>), (<|>))
import           Data.List.Split             (splitOn)
import           Data.Maybe                  (mapMaybe)
import           Data.Monoid                 ((<>))
import           Text.Read                   (readMaybe)

import           Data.Aeson
import           Data.Array                  (bounds, (!))
import qualified Data.Map                    as Map

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BSC
import           Data.Text                   (Text)
import qualified Data.Text                   as T

-- Regular expression parsing, replacement, matching
import           Data.Attoparsec.Text        (Parser, char, endOfInput,
                                              parseOnly, string, takeWhile1)
import           Data.Char                   (isDigit)
import           Text.Regex.TDFA             (MatchText, makeRegex,
                                              matchOnceText)
import           Text.Regex.TDFA.Common      (Regex (..))

-- Reverse proxy apparatus
import           Network.HTTP.Types
import           Network.URI                 (URI (..), nullURI,
                                              parseRelativeReference)

getGroup :: MatchText String -> Int -> String
getGroup matches i = fst $ matches ! i

rewrite :: Char -> (String, MatchText String, String) -> String -> String ->
           Text
rewrite varPrefix (before, match, after) input replacement =
  case parseOnly parseSubstitute (T.pack replacement) of
    Left _ -> T.pack input
    Right result -> T.pack before <> result <> T.pack after
  where
    parseSubstitute :: Parser Text
    parseSubstitute =
          (endOfInput >> "")
      <|> do
          { _ <- string "\\\\"
          ; rest <- parseSubstitute
          ; return $ "\\" <> rest
          }
      <|> do
          { _ <- char varPrefix
          ; n <- (read . T.unpack <$> takeWhile1 isDigit) :: Parser Int
          ; rest <- parseSubstitute
          ; return $ T.pack (getGroup match n) <> rest
          }
      <|> do
          { text <- takeWhile1 (/= varPrefix)
          ; rest <- parseSubstitute
          ; return $ text <> rest
          }

----------------------------------------------------------------------------------
-- |
-- | Rewriting path
-- |
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- | Types, records and instances
----------------------------------------------------------------------------------

-- | Which parts of URI get matched
data MatchType = PathOnly | PathQuery | HostPath | HostPathQuery deriving (Eq,Ord)

instance Show MatchType where
  show PathOnly      = "path-only"
  show PathQuery     = "path-query"
  show HostPath      = "host-path"
  show HostPathQuery = "host-path-query"

instance Read MatchType where
  readsPrec _ s = case s of
    "path-only"       -> [(PathOnly,"")]
    "path-query"      -> [(PathQuery,"")]
    "host-path"       -> [(HostPath,"")]
    "host-path-query" -> [(HostPathQuery,"")]
    _                 -> []

-- | Just optimization, for storing compiled regex in structure
newtype PathRegex = PathRegex {unPathRegex :: Regex}
instance Show PathRegex where show _        = "RewriteRule regex"
instance Read PathRegex where readsPrec _ s = [(PathRegex $ makeRegex s,"")]
-- | Eq and Ord needed for RewritePath which is part of ReverseProxyConfig
instance Eq   PathRegex where _a == _b      = False   -- Should this be implemented?
instance Ord  PathRegex where _a <= _b      = False   -- Should this be implemented?


-- | Info needed for declaring rewrite rule
data RewritePath = RewritePath
    { pathMatchType   :: MatchType    -- ^ which parts of URI are passed for matching
    , pathRegex       :: PathRegex    -- ^ regex for matching specified URI parts
    , pathReplacement :: String       -- ^ new URI
    } deriving (Eq, Ord, Show)

instance FromJSON RewritePath where
  parseJSON = withObject "RewritePath" $ \o -> do
    pathMatchType   <- read <$> o .: "match-type"
    pathRegex       <- read <$> o .: "if-matches"
    pathReplacement <-          o .: "rewrite-to"
    if checkRegexVars pathRegex pathReplacement
      then return RewritePath{..}
      else fail "Incorrect var indexes. 'if-matches' and 'rewrite-to' must match!"

instance ToJSON RewritePath where
    toJSON RewritePath {..} = object
        [ "match-type" .= show pathMatchType
        , "if-matches" .= show pathRegex
        , "rewrite-to" .= pathReplacement
        ]

----------------------------------------------------------------------------------
-- | Functions
----------------------------------------------------------------------------------

rewritePathParts :: [RewritePath] -> (ByteString, ByteString) ->
                    Maybe (ByteString, ByteString)
rewritePathParts rules (path, query) = mkPair <$> mUri
  where
    mkPair uri = (BSC.pack $ uriPath uri, BSC.pack $ uriQuery uri)
    mUri    = rewritePathRule rules rewURI
    rewURI =
      nullURI { uriAuthority = Nothing
              , uriPath      = BSC.unpack path
              , uriQuery     = BSC.unpack query
              }

-- | Match URI with RewritePath rules and rewrite URL
--   Rules are matched as "first-match-wins"
--
--   Matching types:
--  - "path-only"    - Matched:   only path
--                     Rewritten: only path
--      example: /bar/baz
--            >> /baz/bar
--  - "path-query"   - Matched:   path and query
--                     Rewritten: path and query
--      example: /bar/baz/?query=Today
--            >> /baz/bar/?today=Query
--  - "host-path"    - Matched:   host and path
--                     Rewritten: only path
--                     Host is taken from "reversed-host"
--                     New query is merged with Old query.
--                     New query takes precedence, if equal exists in both.
--                     Queries are reordered alphabetically.
--      example: //sub-dom.foo.com/bar/baz
--            >> //foo.com/baz/bar?a=sub-dom
--  - "host-path-qs" - Matched:   host, path and query
--                     Rewritten: path and query
--                     Host is taken from "reversed-host"
--                     New query is merged with Old query.
--                     New query takes precedence, if equal exists in both.
--                     Queries are reordered alphabetically.
--      example: //sub-dom.foo.com/bar/baz?a=1&b=2&c
--            >> //foo.com/baz/bar?a=sub-dom&b=1&c=
rewritePathRule :: [RewritePath] -> URI -> Maybe URI
rewritePathRule []     _           = Nothing
rewritePathRule (x:xs) uri@URI{..} = regexPath x <|> rewritePathRule xs uri
  where
    regexPath RewritePath{..} =
      case pathMatchType of
        HostPath      -> mergeQueries uri <$> goH  (show uriHostPath)
        HostPathQuery -> mergeQueries uri <$> goH  (show uriHostPathQuery)
        PathQuery     -> mergeQueries uri <$> goPQ (show uriPathQuery)
        PathOnly      ->                      goP        uriPath
      where
        goP sMatch  = (\p -> uri{uriPath = subst uriPath p}) <$> doMatch sMatch
        goH sMatch  = doMatch sMatch >>= parseRelativeReference . subst sMatch
        goPQ sMatch = doMatch sMatch >>= parseRelativeReference . subst sMatch
                                     >>= (\u -> Just u{uriAuthority = uriAuthority})
        doMatch     = matchOnceText (unPathRegex pathRegex)

        uriHostPath        = nullURI    {uriPath  = uriPath, uriAuthority = uriAuthority}
        uriPathQuery       = nullURI    {uriPath  = uriPath, uriQuery = uriQuery}
        uriHostPathQuery   = uriHostPath{                    uriQuery = uriQuery}

        subst sMatch match = T.unpack $ rewrite '$' match sMatch pathReplacement


-- | Merge two queries. Keep NEW query item, and remove OLD query item, if both exist.
mergeQueries :: URI -> URI -> URI
mergeQueries old new = new{uriQuery = BSC.unpack mkQuery}
  where
    mkQuery = renderSimpleQuery True . Map.toList
               $ queryToMap new `Map.union` queryToMap old
    queryToMap = foldr go Map.empty . parseSimpleQuery . BSC.pack . uriQuery
      where
        go (key,value) = Map.insert key value


-- | Check if rewrite rules fall inside regex bopunds
checkRegexVars :: PathRegex -> String -> Bool
checkRegexVars PathRegex{..} s = all (\i -> mn <= i && i <= mx) $ listVars s
  where
    (mn,mx)     = bounds $ regex_groups unPathRegex
    listVars xs = mapMaybe (readMaybe . takeWhile isDigit) $ splitOn "$" xs
