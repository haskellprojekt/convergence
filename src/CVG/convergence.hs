module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Snap.Http.Server.Config
import qualified Data.ByteString.Char8 as BS
import Data.String
import Types.Fingerprint
import Backend
import Control.Monad.IO.Class
import Database
import Database.SQLite
import qualified OpenSSL.Session as SSL

certificateFile = "cert.pem"
privateKeyFile = "private.pem"

main :: IO ()
main = start

start :: IO ()
start = do
    db <- Database.connect
    httpServe serverConfig $ route [
        --("target/:host+:port", method POST doCheck),
        (BS.pack "target/:host", method GET (doQuery db))]

serverConfig :: MonadSnap m => Config m a
serverConfig = setSSLKey privateKeyFile $
             setSSLCert certificateFile $
             setSSLPort 8002 $ defaultConfig

getRequest :: String -> (String, Int)
getRequest t = (getHost t, read $ getPort t)
           where
                getHost = takeWhile (/= ' ')
                getPort = dropWhile (/= ' ') . drop 1

doQuery :: SQLiteHandle -> Snap ()
doQuery db = do
        param <- getParam $ BS.pack "host"
        --maybe (writeBS "must specify host param in URL")
        let (host, port) = Main.getRequest $ maybe "" BS.unpack param
        fp <- liftIO $ getFingerprints db host port
        writeBS $ BS.pack $ concat $ map fpJSON fp

