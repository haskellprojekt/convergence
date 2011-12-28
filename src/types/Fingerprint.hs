
module FingerprintItem (FingerprintItem, fpItemHuman, fpItemJSON) where

import Timestamp
import Fingerprint

-- | 'FingerprintItem' defines a tupel of timestamp and fingerprint
type FingerprintItem = (Timestamp, Fingerprint)

-- | 'fpItemHuman' generates a human readable representation of the fingerprint item's data
fpItemHuman :: FingerprintItem -> String
fpItemHuman (t, f) = "Fingerprint Item:\n   " ++ tsHuman t ++ "\n   " ++ fpHuman f ++ "\n"

-- | 'fpItemJSON' generates a JSON representation of the fingerprint item's data
fpItemJSON :: FingerprintItem -> String
fpItemJSON (t, f) = "{\n   " ++ tsJSON t ++ ",\n   " ++ fpJSON f ++ "\n}"
