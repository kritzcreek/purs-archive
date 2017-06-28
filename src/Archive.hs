module Archive where

import Protolude

import qualified Data.Text as Text
import Manifest (Manifest(..), prettyPrintManifest)
import System.FilePath ((</>))

dataDirectory :: FilePath
dataDirectory = "data"

persistManifest :: Manifest -> IO ()
persistManifest m = do
  writeFile (dataDirectory </> fileName) (prettyPrintManifest m)
    where
      fileName =
        Text.unpack (manifestName m <> "-" <> manifestVersion m <> ".toml")

lol :: Text
lol = "lol"
