module I where

-- GHCI helpers

import Protolude
import Network.Wreq

uploadManifest :: IO (Response LByteString)
uploadManifest = post "http://localhost:3000/upload" (partFile "file" "manifest.toml")
