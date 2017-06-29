module I where

-- GHCI helpers

import Protolude hiding (get, put)
import Control.Lens ((^.))
import qualified Codec.Archive.Tar as Tar
import Network.Wreq

upload :: IO (Response LByteString)
upload = post "http://localhost:3000/package" (partFile "file" "manifest.toml")

listPackages :: IO (Response LByteString)
listPackages = get "http://localhost:3000/package"

createIndex :: IO (Response LByteString)
createIndex = get "http://localhost:3000/createIndex"

getIndex :: IO ()
getIndex = do
  r <- get "http://localhost:3000/index"
  let body = r ^. responseBody
  Tar.unpack "result" (Tar.read body)
