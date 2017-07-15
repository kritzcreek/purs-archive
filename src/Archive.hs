module Archive (persistManifest, listPackages, createIndex) where

import Protolude

import qualified Data.Text as Text
import qualified Data.SemVer as SemVer
import qualified Codec.Archive.Tar as Tar
import Manifest (Manifest(..), PackageName, prettyPrintManifest)
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import Persist (EntityField(PackageName), packageOwner, Package(..), Version(..))
import Database.Persist.Sqlite

dataDirectory :: FilePath
dataDirectory = "data"

persistManifest :: Text -> Manifest -> SqlPersistM ()
persistManifest email m = do
  liftIO saveFile
  persistPackage
  where
    pkgName = manifestName m

    persistPackage = do
      r <- selectFirst [PackageName ==. pkgName] []
      case r of
        Nothing ->
          createNewPackage
        Just p ->
          if packageOwner (entityVal p) /= email then
            traceM ("Package already taken by: " <> email)
          else
            addNewPackageVersion
      pure ()

    addNewPackageVersion =
      void (insert (Version pkgName (SemVer.toText (manifestVersion m))))

    createNewPackage = do
      _ <- insert (Package pkgName email)
      _ <- insert (Version pkgName (SemVer.toText (manifestVersion m)))
      pure ()
    saveFile = do
      let dirName = dataDirectory </> Text.unpack (manifestName m)
      createDirectoryIfMissing True dirName
      writeFile (dirName </> Text.unpack (SemVer.toText (manifestVersion m) <> ".toml")) (prettyPrintManifest m)

listPackages :: SqlPersistM [PackageName]
listPackages = do
  packages <- selectList [] []
  pure (map (packageName . entityVal) packages)

createIndex :: IO ()
createIndex = do
  whenM (doesDirectoryExist dataDirectory) $ do
   Tar.create "index.tar" dataDirectory =<< listDirectory dataDirectory
