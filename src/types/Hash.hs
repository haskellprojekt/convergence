
module Hash (Hash, fromX509, hashHuman, hashJSON) where

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


-- | 'Hash' defines a List of Word8's
type Hash = [Word8]

-- | 'hashHuman' generates a human readable representation of the hash's data
hashHuman :: Hash -> String
hashHuman f = "Hash: " ++ hash2String f

-- | 'hashJSON' generates a JSON representation of the hash's data
hashJSON :: Hash -> String
hashJSON f = "\"hash\": \"" ++ hash2String f ++ "\""

fromX509 :: X509 -> IO Hash
fromX509 x509 = do
    pem <- writeX509 x509
    hash <- sha1 $ decode $ cutPem pem
    return hash
    where
        -- cuts "--Begin..." and "---End"
        cutPem :: String -> String
        cutPem pem = concat $ init $ tail $ splitOn "\n" pem

sha1 :: String -> IO Hash 
sha1 str = do
    dig <- getDigestByName "sha1"
    let hash = digest (fromJust dig) str
    return $ UTF8.encode $ hash


-- | 'hash2String' generates a String representating the hash's data
hash2String :: Hash -> String
hash2String [] = ""
hash2String (f:fs)
	| [] == fs  = x
	| otherwise = x ++ ":" ++ hash2String fs
	where x = lenTo2 (upperCase (toHex f))
--

-- | 'toHex' calculates the hexadecimal representation of a given number
toHex :: Integral a => a -> String
toHex n = showIntAtBase 16 intToDigit n ""

-- | 'upperCase' converts a String to upper case
upperCase :: String -> String
upperCase = map toUpper

-- | 'lenTo2' adds a missing leading 0
lenTo2 :: String -> String
lenTo2 [] = "00"
lenTo2 (n:[]) = '0' : n : []
lenTo2 nx = nx
