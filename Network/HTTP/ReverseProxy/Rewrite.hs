{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.HTTP.ReverseProxy.Rewrite
  ( ReverseProxyConfig (..)
  , RewriteRule (..)
  , RPEntry (..)
  , simpleReverseProxy
  )
  where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Exception           (bracket)
import           Data.Function               (fix)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))

import           Control.Monad               (unless)
import           Data.Aeson
import           Data.Array                  ((!))
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as BSC
import qualified Data.CaseInsensitive        as CI
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import           Blaze.ByteString.Builder    (fromByteString)

-- Configuration files
import           Data.Default

-- Regular expression parsing, replacement, matching
import           Text.Regex.TDFA             (MatchText, makeRegex,
                                              matchOnceText)
import           Text.Regex.TDFA.Common      (Regex (..))
import           Keter.Proxy.Rewrite         (RewritePath, rewrite,
                                              rewritePathRule)

-- Reverse proxy apparatus
import qualified Network.HTTP.Client         as NHC
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types
import           Network.URI                 (URI (..), URIAuth (..), nullURI)
import qualified Network.Wai                 as Wai

data RPEntry = RPEntry
    { config      :: ReverseProxyConfig
    , httpManager :: Manager
    }

instance Show RPEntry where
  show x = "RPEntry { config = " ++ (show $ config x) ++ " }"

rewriteHeader :: Map HeaderName RewriteRule -> Header -> Header
rewriteHeader rules header@(name, value) =
  case Map.lookup name rules of
    Nothing -> header
    Just  r -> (name, regexRewrite r value)

rewriteHeaders :: Map HeaderName RewriteRule -> [Header] -> [Header]
rewriteHeaders ruleMap = map (rewriteHeader ruleMap)

regexRewrite :: RewriteRule -> S.ByteString -> S.ByteString
regexRewrite (RewriteRule _ regex' replacement) input =
  case matchOnceText regex strInput of
    Just  match -> encodeUtf8 $ rewrite '\\' match strInput strReplacement
    Nothing     -> input
  where
    strRegex = T.unpack regex'
    regex :: Regex
    regex = makeRegex strRegex
    strInput = T.unpack . decodeUtf8 $ input
    strReplacement = T.unpack replacement

filterHeaders :: [Header] -> [Header]
filterHeaders = filter useHeader
  where
    useHeader ("Transfer-Encoding", _) = False
    useHeader ("Content-Length", _)    = False
    useHeader ("Host", _)              = False
    useHeader _                        = True

mkRuleMap :: Set RewriteRule -> Map HeaderName RewriteRule
mkRuleMap = Map.fromList . map (\k -> (CI.mk . encodeUtf8 $ ruleHeader k, k)) . Set.toList

mkRequest :: ReverseProxyConfig -> Wai.Request -> (Request,Maybe URI)
mkRequest rpConfig request =
  (def {
     method         = Wai.requestMethod request
   , secure         = reverseUseSSL rpConfig
   , host           = BSC.pack host
   , port           = reversedPort rpConfig
   , path           = BSC.pack $ uriPath  uri
   , queryString    = BSC.pack $ uriQuery uri
   , requestHeaders = filterHeaders $ rewriteHeaders reqRuleMap headers
   , requestBody    =
       case Wai.requestBodyLength request of
         Wai.ChunkedBody   -> RequestBodyStreamChunked ($ Wai.requestBody request)
         Wai.KnownLength n -> RequestBodyStream (fromIntegral n) ($ Wai.requestBody request)
   , decompress      = const False
   , redirectCount   = 10 -- FIXMEE: Why is this reduced to 0 from default 10???
   , checkStatus     = \_ _ _ -> Nothing
   , responseTimeout = reverseTimeout rpConfig
   , cookieJar       = Nothing
   }
  , mkURI)
  where
    headers    = Wai.requestHeaders request
    mkURI      = rewritePathRule (rewritePath rpConfig) rewURI
    uri        = fromMaybe rewURI mkURI
    reqRuleMap = mkRuleMap $ rewriteRequestRules rpConfig
    host       = T.unpack  $ reversedHost        rpConfig
    rewURI     =
      nullURI{uriAuthority = Just $ URIAuth ""
                                            (maybe host BSC.unpack $ lookup "Host" headers)
                                            "",
              uriPath      = BSC.unpack $ Wai.rawPathInfo    request,
              uriQuery     = BSC.unpack $ Wai.rawQueryString request}


simpleReverseProxy :: Manager -> ReverseProxyConfig -> Wai.Application
simpleReverseProxy mgr rpConfig request sendResponse = bracket
    (NHC.responseOpen proxiedRequest mgr)
    (\res -> do
        responseClose res
        case mRewrite of
          Just rp -> putStrLn $ "Rewrite path: " <> show rp
          _       -> return ())
    $ \res -> sendResponse $ Wai.responseStream
        (responseStatus res)
        (rewriteHeaders respRuleMap $ responseHeaders res)
        (sendBody $ responseBody res)
  where
    (proxiedRequest,mRewrite) = mkRequest rpConfig request
    respRuleMap = mkRuleMap $ rewriteResponseRules rpConfig
    sendBody body send _flush = fix $ \loop -> do
        bs <- body
        unless (S.null bs) $ do
            () <- send $ fromByteString bs
            loop

data ReverseProxyConfig = ReverseProxyConfig
    { reversedHost         :: Text
    , reversedPort         :: Int
    , reversingHost        :: Text
    , reverseUseSSL        :: Bool
    , reverseTimeout       :: Maybe Int
    , rewriteResponseRules :: Set RewriteRule
    , rewriteRequestRules  :: Set RewriteRule
    , rewritePath          :: [RewritePath]
    } deriving (Eq, Ord, Show)

instance FromJSON ReverseProxyConfig where
    parseJSON (Object o) = ReverseProxyConfig
        <$> o .: "reversed-host"
        <*> o .: "reversed-port"
        <*> o .: "reversing-host"
        <*> o .:? "ssl" .!= False
        <*> o .:? "timeout" .!= Nothing
        <*> o .:? "rewrite-response" .!= Set.empty
        <*> o .:? "rewrite-request" .!= Set.empty
        <*> o .:? "rewrite-path"     .!= []
    parseJSON _ = fail "Wanted an object"

instance ToJSON ReverseProxyConfig where
    toJSON ReverseProxyConfig {..} = object
        [ "reversed-host" .= reversedHost
        , "reversed-port" .= reversedPort
        , "reversing-host" .= reversingHost
        , "ssl" .= reverseUseSSL
        , "timeout" .= reverseTimeout
        , "rewrite-response" .= rewriteResponseRules
        , "rewrite-request" .= rewriteRequestRules
        , "rewrite-path"     .= rewritePath
        ]

instance Default ReverseProxyConfig where
    def = ReverseProxyConfig
        { reversedHost = ""
        , reversedPort = 80
        , reversingHost = ""
        , reverseUseSSL = False
        , reverseTimeout = Nothing
        , rewriteResponseRules = Set.empty
        , rewriteRequestRules = Set.empty
        , rewritePath          = []
        }

data RewriteRule = RewriteRule
    { ruleHeader      :: Text
    , ruleRegex       :: Text
    , ruleReplacement :: Text
    } deriving (Eq, Ord, Show)

instance FromJSON RewriteRule where
    parseJSON (Object o) = RewriteRule
        <$> o .: "header"
        <*> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"

instance ToJSON RewriteRule where
    toJSON RewriteRule {..} = object
        [ "header" .= ruleHeader
        , "from" .= ruleRegex
        , "to" .= ruleReplacement
        ]
