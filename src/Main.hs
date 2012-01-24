module Main (main) where

import CVG.Main
import CVG.Config


main :: IO ()
main = CVG.Main.start defaultConfig {
     ports = Ports { plain_port = Just 8080, ssl_port = Just 8443}
}
