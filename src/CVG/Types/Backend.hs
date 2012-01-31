module CVG.Types.Backend where

import CVG.Types.Fingerprint

data Host = Host String Int deriving Show

data Response = Response [Fingerprint] deriving Show
data Request = Request
     { hostname :: Host
     , fingerprint :: Maybe String
     }
