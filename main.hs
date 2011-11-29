module Main where

import OpenSSL
import OpenSSL.EVP.Digest

main :: IO ()
main = withOpenSSL $
     do
     	foo <- getDigestNames
        print foo
