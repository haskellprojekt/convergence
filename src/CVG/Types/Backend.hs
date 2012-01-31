module CVG.Types.Backend where

import CVG.Types.Fingerprint

data Host = Host String Int deriving Show

data Response = Response [Fingerprint] deriving Show
data Request = Request
     { hostname :: Host
     , fingerprint :: Maybe String
     }

toJSON :: Response -> IO String
toJSON (Response fps) = return $ concat $ map fpJSON fps
