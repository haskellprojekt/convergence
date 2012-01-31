module CVG.Backend (isFingerprintOk, getFingerprints, queryFingerprint, simpleScan)
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
isFingerprintOk :: SQLiteHandle -> String -> Int -> Fingerprint -> IO Bool
isFingerprintOk db host port fp = do
    fps <- getFingerprints db host port
    return $ elem fp fps

-- | get the fingerprints for a host and port. First it looks at the Database.
-- | If there is nothing found, request the SSL certificate and return its fingerprint
getFingerprints :: SQLiteHandle -> String -> Int -> IO [Fingerprint]
getFingerprints db host port = withOpenSSL $ do
    fps <- CVG.Database.findFingerprints db host port
    if null fps
      then do
        qfp <- queryFingerprint host port
        CVG.Database.insert db host port qfp
        return [qfp]
      else
        return fps

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

