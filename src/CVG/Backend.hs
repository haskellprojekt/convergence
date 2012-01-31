module CVG.Backend (queryFingerprint, simpleScan)
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
import CVG.Types.Fingerprint
import CVG.Database
import Database.SQLite
import CVG.Types.Backend

-- | checks if a fingerprint is stored in the database
-- isFingerprintOk :: SQLiteHandle -> Host -> Fingerprint -> IO Bool
-- isFingerprintOk db host fp = do
--     fps <- getFingerprints db host
--     return $ elem fp fps

simpleScan :: Request -> IO Response
simpleScan (Request host Nothing) = do
    fpr <- queryFingerprint host
    return $ Response [fpr]
simpleScan (Request host (Just fp)) = do
    error "Not implemented"

-- | starts a SSL connection to a host on port 443 and gives his fingerprint back
queryFingerprint :: String -> Int -> IO Fingerprint
queryFingerprint host port  = withOpenSSL $ do
    addrs <- getAddrInfo (Just defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME,AI_NUMERICSERV] }) (Just host) (Just (show port))
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

