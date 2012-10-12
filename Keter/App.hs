{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.App
    ( App
    , start
    , reload
    , Keter.App.terminate
    ) where

import Prelude (IO)
import Keter.Prelude
import Keter.TempFolder
import Keter.Postgres
import Keter.Process
import Keter.Logger (Logger, detach)
import Keter.PortManager hiding (start)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Codec.Compression.GZip (decompress)
import qualified Filesystem.Path.CurrentOS as F
import Data.Yaml
import Control.Applicative ((<$>), (<*>))
import System.PosixCompat.Files
import qualified Network
import Data.Maybe (fromMaybe)
import Control.Exception (onException, throwIO)
import System.IO (hClose)
import qualified Data.ByteString.Lazy as L
import Data.Conduit (($$), yield, runResourceT)
import Data.Conduit.Binary (sinkFile)
import Data.Set (Set)
import qualified Data.Set as Set

data Config = Config
    { configExec :: F.FilePath
    , configArgs :: [Text]
    , configHost :: String
    , configPostgres :: Bool
    , configSsl :: Bool
    , configExtraHosts :: Set String
    }

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> (F.fromText <$> o .: "exec")
        <*> o .:? "args" .!= []
        <*> o .: "host"
        <*> o .:? "postgres" .!= False
        <*> o .:? "ssl" .!= False
        <*> o .:? "extra-hosts" .!= Set.empty
    parseJSON _ = fail "Wanted an object"

data Command = Reload | Terminate
newtype App = App (Command -> KIO ())

unpackBundle :: TempFolder
             -> F.FilePath
             -> Appname
             -> KIO (Either SomeException (FilePath, Config))
unpackBundle tf bundle appname = do
    elbs <- readFileLBS bundle
    case elbs of
        Left e -> return $ Left e
        Right lbs -> do
            edir <- getFolder tf appname
            case edir of
                Left e -> return $ Left e
                Right dir -> do
                    log $ UnpackingBundle bundle dir
                    let rest = do
                            unpackTar dir $ Tar.read $ decompress lbs
                            let configFP = dir F.</> "config" F.</> "keter.yaml"
                            Just config <- decodeFile $ F.encodeString configFP
                            return (dir, config)
                    liftIO $ rest `onException` removeTree dir

unpackTar :: FilePath -> Tar.Entries Tar.FormatError -> IO ()
unpackTar dir =
    loop . Tar.checkSecurity
  where
    loop Tar.Done = return ()
    loop (Tar.Fail e) = either throwIO throwIO e
    loop (Tar.Next e es) = go e >> loop es

    go e = do
        let fp = dir </> decodeString (Tar.entryPath e)
        case Tar.entryContent e of
            Tar.NormalFile lbs _ -> do
                createTree $ F.directory fp
                runResourceT $ mapM_ yield (L.toChunks lbs) $$ sinkFile (F.encodeString fp)
                setFileMode (F.encodeString fp) $ Tar.entryPermissions e
            _ -> return ()

start :: TempFolder
      -> PortManager
      -> Postgres
      -> Logger
      -> Appname
      -> F.FilePath -- ^ app bundle
      -> KIO () -- ^ action to perform to remove this App from list of actives
      -> KIO (App, KIO ())
start tf portman postgres logger appname bundle removeFromList = do
    chan <- newChan
    return (App $ writeChan chan, rest chan)
  where
    runApp port dir config = do
        otherEnv <- do
            mdbi <-
                if configPostgres config
                    then do
                        edbi <- getInfo postgres appname
                        case edbi of
                            Left e -> do
                                $logEx e
                                return Nothing
                            Right dbi -> return $ Just dbi
                    else return Nothing
            return $ case mdbi of
                Just dbi ->
                    [ ("PGHOST", "localhost")
                    , ("PGPORT", "5432")
                    , ("PGUSER", dbiUser dbi)
                    , ("PGPASS", dbiPass dbi)
                    , ("PGDATABASE", dbiName dbi)
                    ]
                Nothing -> []
        let env = ("PORT", show port)
                : ("APPROOT", (if configSsl config then "https://" else "http://") ++ configHost config)
                : otherEnv
        run
            ("config" </> configExec config)
            dir
            (configArgs config)
            env
            logger

    rest chan = forkKIO $ do
        mres <- unpackBundle tf bundle appname
        case mres of
            Left e -> do
                $logEx e
                removeFromList
            Right (dir, config) -> do
                eport <- getPort portman
                case eport of
                    Left e -> do
                        $logEx e
                        removeFromList
                    Right port -> do
                        process <- runApp port dir config
                        b <- testApp port
                        if b
                            then do
                                addEntry portman (configHost config) port
                                mapM_ (flip (addEntry portman) port) $ Set.toList $ configExtraHosts config
                                loop chan dir process port config
                            else do
                                removeFromList
                                releasePort portman port
                                Keter.Process.terminate process

    loop chan dirOld processOld portOld configOld = do
        command <- readChan chan
        case command of
            Terminate -> do
                removeFromList
                removeEntry portman $ configHost configOld
                mapM_ (removeEntry portman) $ Set.toList $ configExtraHosts configOld
                log $ TerminatingApp appname
                terminateOld
                detach logger
            Reload -> do
                mres <- unpackBundle tf bundle appname
                case mres of
                    Left e -> do
                        log $ InvalidBundle bundle e
                        loop chan dirOld processOld portOld configOld
                    Right (dir, config) -> do
                        eport <- getPort portman
                        case eport of
                            Left e -> $logEx e
                            Right port -> do
                                process <- runApp port dir config
                                b <- testApp port
                                if b
                                    then do
                                        addEntry portman (configHost config) port
                                        mapM_ (flip (addEntry portman) port) $ Set.toList $ configExtraHosts config
                                        when (configHost config /= configHost configOld) $
                                            removeEntry portman $ configHost configOld
                                        log $ FinishedReloading appname
                                        terminateOld
                                        loop chan dir process port config
                                    else do
                                        releasePort portman port
                                        Keter.Process.terminate process
                                        log $ ProcessDidNotStart bundle
                                        loop chan dirOld processOld portOld configOld
      where
        terminateOld = forkKIO $ do
            threadDelay $ 20 * 1000 * 1000
            log $ TerminatingOldProcess appname
            Keter.Process.terminate processOld
            threadDelay $ 60 * 1000 * 1000
            log $ RemovingOldFolder dirOld
            res <- liftIO $ removeTree dirOld
            case res of
                Left e -> $logEx e
                Right () -> return ()

testApp :: Port -> KIO Bool
testApp port = do
    res <- timeout (90 * 1000 * 1000) testApp'
    return $ fromMaybe False res
  where
    testApp' = do
        threadDelay $ 2 * 1000 * 1000
        eres <- liftIO $ Network.connectTo "127.0.0.1" $ Network.PortNumber $ fromIntegral port
        case eres of
            Left _ -> testApp'
            Right handle -> do
                res <- liftIO $ hClose handle
                case res of
                    Left e -> $logEx e
                    Right () -> return ()
                return True

reload :: App -> KIO ()
reload (App f) = f Reload

terminate :: App -> KIO ()
terminate (App f) = f Terminate
