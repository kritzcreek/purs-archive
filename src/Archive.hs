module Archive
  ( persistManifest
  , listPackages
  , createIndex
  , PersistError(..)
  ) where

import Protolude hiding (packageName)

import qualified Data.Text as Text
import qualified Data.SemVer as SemVer
import qualified Codec.Archive.Tar as Tar
import Manifest (Manifest(..), PackageName, prettyPrintManifest)
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import Persist (EntityField(..), packageOwner, Package(..), Version(..))
import Database.Persist.Sqlite

dataDirectory :: FilePath
dataDirectory = "data"

data PersistError = PackageAlreadyTaken | VersionAlreadyUploaded

persistManifest :: Text -> Manifest -> SqlPersistM (Maybe PersistError)
persistManifest email m = do
  liftIO saveFile
  persistPackage
  where
    pkgName = manifestName m

    pkgVersion = SemVer.toText (manifestVersion m)

    persistPackage = do
      r <- selectFirst [PackageName ==. pkgName] []
      case r of
        Nothing ->
          createNewPackage $> Nothing
        Just p | packageOwner (entityVal p) /= email -> pure (Just PackageAlreadyTaken)
        Just _ -> do
          versionTaken <- isJust <$> selectFirst [ VersionPackage ==. pkgName, VersionVersion ==. pkgVersion ] []
          if versionTaken
            then pure (Just VersionAlreadyUploaded)
            else addNewPackageVersion $> Nothing

    addNewPackageVersion =
      void (insert (Version pkgName pkgVersion))

    createNewPackage = do
      _ <- insert (Package pkgName email)
      insert (Version pkgName (SemVer.toText (manifestVersion m)))

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
