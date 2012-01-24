module CVG.Main (start) where

import OpenSSL
import qualified OpenSSL.Session as SSL
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Http.Server.Config
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String
import CVG.Types.Fingerprint
import CVG.Backend
import Control.Monad.IO.Class
import CVG.Database
import Database.SQLite
import qualified OpenSSL.Session as SSL
import CVG.Config hiding ( defaultConfig )
import Data.List.Utils

start :: CConfig -> IO ()
start config = withOpenSSL $ do
      db <- CVG.Database.connect $ sqlite_db config
      ssl <- sslConfig config

      httpServe (serverConfig config) $ route [
                --(BS.pack "target/:host", method POST (doCheck ssl db)),
                (BS.pack "target/:host", method GET (doQuery db))]

sslConfig :: CConfig -> IO SSL.SSLContext
sslConfig config = do 
      ssl <- SSL.context
      SSL.contextSetPrivateKeyFile ssl $ ssl_key config
      SSL.contextSetCertificateFile ssl $ ssl_cert config
      SSL.contextSetDefaultCiphers ssl
      return ssl

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

doQuery :: SQLiteHandle -> Snap ()
doQuery db = do
        param <- getParam $ BS.pack "host"
        --maybe (writeBS "must specify host param in URL")
        let (host, port) = CVG.Main.getRequest $ maybe "" BS.unpack param
        fp <- liftIO $ getFingerprints db host port
        writeBS $ BS.pack $ concat $ map fpJSON fp
{-
doCheck :: SSL.SSLContext -> SQLiteHandle -> Snap()
doCheck ssl db = do
    param <- getParam $ BS.pack "host"

    let (host,port) = CVG.Main.getRequest $ maybe "" BS.unpack param
    
    body <- readRequestBody 512
    let bodystr = BSL.unpack body
    let fp :: String = "fingerprint=" "" bodystr
   
    writeBS $ BS.pack ("host:" ++ host ++ "\nport:" ++ show port ++ "\nfingerprint:" ++ fp )
-}
