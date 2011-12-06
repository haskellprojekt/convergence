import Network
import Network.Socket
import OpenSSL
import OpenSSL.Session
import OpenSSL.X509
import OpenSSL.PEM
import OpenSSL.EVP.Digest
import OpenSSL.EVP.Base64
import Maybe
import Codec.Binary.Base64.String
import Numeric
import Data.List.Split
import Data.String.Utils
import Char

-- Konvertiert eine Zahl in die Hexidecimal Schreibweise
toHex :: Int -> Int -> String
toHex n l
        | l > length(hex) = '0':(toHex n (l-1))
        | otherwise = hex
        where hex = replace "\"" "" (show(showHex n ""))

hashing :: String -> String -> IO String
hashing algo str = do
        dig <- getDigestByName algo
        let hash = digest (fromJust dig) str
        return (concat(map (\it -> toHex (ord it) 2)  hash))

fingerprintX509 :: String -> X509 -> IO String
fingerprintX509 algo x509 = do
    pem <- writeX509 x509
    hash <- hashing algo $ decode $ cutPem pem
    return hash
    where
        -- cuts "--Begin..." and "---End"
        cutPem :: String -> String
        cutPem pem = concat $ init $ tail $ splitOn "\n" pem

fingerprint :: String -> Int -> IO String
fingerprint domain port  = withOpenSSL $ do
    addrs <- getAddrInfo (Just defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME,AI_NUMERICSERV] }) (Just domain) (Just (show port))
    let addr = head addrs

    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    Network.Socket.connect sock (addrAddress addr)

    sslctx <- context
    contextSetDefaultCiphers sslctx

    sslcon <- connection sslctx sock
    OpenSSL.Session.connect sslcon

    Just cert <- getPeerCertificate sslcon
    text <- printX509 cert
    hash <- (fingerprintX509 "sha1" cert)
    return hash

main = do
    hash <- fingerprint "facebook.com" 443
    putStrLn hash
