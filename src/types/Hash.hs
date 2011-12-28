
module Hash (Hash, hsFromX509, hsHuman, hsJSON) where

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

-- | 'hsHuman' generates a human readable representation of the hash's data
hsHuman :: Hash -> String
hsHuman f = "Hash: " ++ hs2String f

-- | 'hsJSON' generates a JSON representation of the hash's data
hsJSON :: Hash -> String
hsJSON f = "\"hash\": \"" ++ hs2String f ++ "\""

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
sha1 :: String -> IO Hash
sha1 str = do
    dig <- getDigestByName "sha1"
    let hash = digest (fromJust dig) str
    print hash
    return $ map c2w8 hash
-- | 'c2w8' translate a character into a word8
c2w8 :: Char -> Word8
c2w8 chr = (fromIntegral $ ord chr) :: Word8

-- | 'hs2String' generates a String representating the hash's data
hs2String :: Hash -> String
hs2String [] = ""
hs2String (f:fs)
	| [] == fs  = x
	| otherwise = x ++ ":" ++ hs2String fs
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
