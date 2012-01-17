module CVG.Config where

data Ports = Ports { plain_port :: Maybe Int, ssl_port :: Maybe Int }

data CConfig = CConfig
     { ssl_cert :: String
     , ssl_key :: String
     , sqlite_db :: String
     , ports :: Ports
     }

defaultConfig = CConfig
     { ssl_cert = "cert.pem"
     , ssl_key = "private.pem"

     , sqlite_db = "fingerprints.sqlite"

     , ports = Ports { plain_port = Just 80, ssl_port = Just 443 }
     }
