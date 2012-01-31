module CVG.Main (start) where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Http.Server.Config
import qualified Data.ByteString.Char8 as BS
import Data.String
import CVG.Types.Fingerprint
import CVG.Types.Backend
import CVG.Backend
import Control.Monad.IO.Class
import CVG.Database
import Database.SQLite
import qualified OpenSSL.Session as SSL
import CVG.Config hiding ( defaultConfig )

start :: CConfig -> IO ()
start config = do
      httpServe (serverConfig config) $ route [
                --("target/:host+:port", method POST doCheck),
                (BS.pack "target/:host", method GET (doQuery config))]

serverConfig :: MonadSnap m => CConfig -> Config m a
serverConfig config = setSSLKey (ssl_key config) $
             setSSLCert (ssl_cert config) $
             snapSetSSLPort (ports config) $
             snapSetPort (ports config) $
             defaultConfig

snapSetSSLPort :: MonadSnap m => Ports -> Config m a -> Config m a
snapSetSSLPort Ports { ssl_port = Just ssl } config = setSSLPort ssl $ config
snapSetSSLPort Ports { ssl_port = Nothing } config = config

snapSetPort :: MonadSnap m => Ports -> Config m a -> Config m a
snapSetPort Ports { plain_port = Just port } config = setPort port $ config
snapSetPort Ports { plain_port = Nothing } config = config


getRequest :: String -> (String, Int)
getRequest t = (getHost t, read $ getPort t)
           where
                getHost = takeWhile (/= ' ')
                getPort = dropWhile (/= ' ') . drop 1

doQuery :: CConfig -> Snap ()
doQuery config = do
        param <- getParam $ BS.pack "host"
        --maybe (writeBS "must specify host param in URL")
        let (host, port) = CVG.Main.getRequest $ maybe "" BS.unpack param
        request <- liftIO $ before config $ Request (Host host port) Nothing
        response <- liftIO $ backend config $ request
        final <- liftIO $ after config $ response
        output <- liftIO $ toJSON $ final
        writeBS $ BS.pack $ output

