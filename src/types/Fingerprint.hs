
module Fingerprint (Fingerprint, fpHuman, fpJSON) where

import Timestamp
import Hash

-- | 'Fingerprint' defines a tupel of timestamp and hash
type Fingerprint = (Timestamp, Hash)

-- | 'fpHuman' generates a human readable representation of the fingerprint's data
fpHuman :: Fingerprint -> String
fpHuman (t, f) = "Fingerprint:\n   " ++ tsHuman t ++ "\n   " ++ fpHuman f ++ "\n"

-- | 'fpJSON' generates a JSON representation of the fingerprint's data
fpJSON :: Fingerprint -> String
fpJSON (t, f) = "{\n   " ++ tsJSON t ++ ",\n   " ++ fpJSON f ++ "\n}"
