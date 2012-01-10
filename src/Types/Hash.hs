-- This module was once named Fingerprint,
-- Hash seems for me more intuitive.
-- 
module Types.Hash (Hash, hsFromX509, hsHuman, hsJSON) where


import Numeric
import Data.Char
import Data.Word
import OpenSSL
import OpenSSL.X509
import OpenSSL.PEM
import OpenSSL.EVP.Digest
import OpenSSL.EVP.Base64
import Maybe
import Codec.Binary.Base64.String
import qualified Codec.Binary.UTF8.String as UTF8
import Numeric
import Data.List.Split
import Data.String.Utils
import Char


-- | 'Hash' defines a Hash String
type Hash = String


-- | 'hsJSON' generates a JSON representation of the hash's data
hsJSON :: Hash -> String
hsJSON hs = "\"hash\": \"" ++ hs ++ "\""

-- | 'hsFromX509' calculates the sha1 hash for a x509 certificate
hsFromX509 :: X509 -> IO Hash
hsFromX509 x509 = do
    pem <- writeX509 x509
    hash <- sha1 $ decode $ cutPem pem
    return hash
    where
        -- cuts "--Begin..." and "---End"
        cutPem :: String -> String
        cutPem pem = concat $ init $ tail $ splitOn "\n" pem
-- | 'sha1' calculates the sha1 hash
sha1 :: String -> IO String
sha1 str = withOpenSSL $ do
    dig <- getDigestByName "sha1"
    let hash = digest (fromJust dig) str
    return $ concat $ map chr2hex hash
-- | 'c2w8' translate a character into a word8
chr2hex :: Char -> String
chr2hex chr = lenTo2 $ toHex $ ord chr

-- | 'toHex' calculates the hexadecimal representation of a given number
toHex :: Integral a => a -> String
toHex n = upperCase $ showIntAtBase 16 intToDigit n ""


hsHuman :: String -> String
hsHuman hs = "Hash: " ++ hs

-- | 'upperCase' converts a String to upper case
upperCase :: String -> String
upperCase = map toUpper

-- | 'lenTo2' adds a missing leading 0
lenTo2 :: String -> String
lenTo2 [] = "00"
lenTo2 (n:[]) = '0' : n : []
lenTo2 nx = nx
