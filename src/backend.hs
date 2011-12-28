module Backend
where
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
import Fingerprint

fingerprint :: String -> Int -> IO Fingerprint
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
    fp <- (fpFromX509 cert)
    return $ fp

