module Types.Fingerprint (Fingerprint, fpFromX509, fpHuman, fpJSON) where

import Types.Timestamp
import Types.Hash
import OpenSSL.X509

-- | 'Fingerprint' defines a tupel of timestamp and hash
type Fingerprint = (Timestamp, Hash)

fpFromX509 :: X509 -> IO Fingerprint
fpFromX509 x509 = do
    hash <- hsFromX509 x509
    timestamp <- tsFromX509 x509
    return (timestamp, hash)

-- | 'fpHuman' generates a human readable representation of the fingerprint's data
fpHuman :: Fingerprint -> String
fpHuman (t, h) = "Fingerprint:\n   " ++ tsHuman t ++ "\n   " ++ hsHuman h ++ "\n"

-- | 'fpJSON' generates a JSON representation of the fingerprint's data
fpJSON :: Fingerprint -> String
fpJSON (t, h) = "{\n   " ++ tsJSON t ++ ",\n   " ++ hsJSON h ++ "\n}"
