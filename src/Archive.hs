module Archive (persistManifest, listPackages, createIndex) where

import Protolude

import qualified Data.Text as Text
import qualified Data.SemVer as SemVer
import qualified Codec.Archive.Tar as Tar
import Manifest (Manifest(..), PackageName, prettyPrintManifest)
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

dataDirectory :: FilePath
dataDirectory = "data"

persistManifest :: Manifest -> IO ()
persistManifest m = do
  let dirName = dataDirectory </> Text.unpack (manifestName m)
  createDirectoryIfMissing True dirName
  writeFile (dirName </> Text.unpack (SemVer.toText (manifestVersion m) <> ".toml")) (prettyPrintManifest m)

listPackages :: IO [PackageName]
listPackages = map Text.pack <$> listDirectory dataDirectory

createIndex :: IO ()
createIndex = do
  whenM (doesDirectoryExist dataDirectory) $ do
   Tar.create "index.tar" dataDirectory =<< listDirectory dataDirectory
