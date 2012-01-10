module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.ByteString.Char8 as BS
import Data.String
import Types.Fingerprint
import Backend
import Control.Monad.IO.Class
import Database
import Database.SQLite

main :: IO ()
main = do
     db <- Database.connect
     quickHttpServe $ route [
         --("target/:host+:port", method POST doCheck),
         ("target/:host", method GET (doQuery db))
     ]

getRequest :: String -> (String, Int)
getRequest t = (getHost t, read $ getPort t)
           where
                getHost = takeWhile (/= ' ')
                getPort = dropWhile (/= ' ') . drop 1

doQuery :: SQLiteHandle -> Snap ()
doQuery db = do
        param <- getParam "host"
        --maybe (writeBS "must specify host param in URL")
        let (host, port) = Main.getRequest $ maybe "" BS.unpack param
        fp <- liftIO $ getFingerprints db host port
        writeBS $ BS.pack $ concat $ map fpJSON fp

