
import Numeric
import Data.Char
import Data.Word

-- | 'Fingerprint' defines a List of Word8's
type Fingerprint = [Word8]

-- | 'fpHuman' generates a human readable representation of the fingerprint's data
fpHuman :: Fingerprint -> String
fpHuman f = "Fingerprint: " ++ fp2String f

-- | 'fpJSON' generates a JSON representation of the fingerprint's data
fpJSON :: Fingerprint -> String
fpJSON f = "\"fingerprint\": \"" ++ fp2String f ++ "\""

-- | 'fp2String' generates a String representating the fingerprint's data
fp2String :: Fingerprint -> String
fp2String [] = ""
fp2String (f:fs)
	| [] == fs  = x
	| otherwise = x ++ ":" ++ fp2String fs
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
