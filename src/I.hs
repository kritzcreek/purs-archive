module I where

-- GHCI helpers

import Protolude

import Control.Lens ((^.), (?~))
import Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Codec.Archive.Tar as Tar
import qualified Network.Wreq as W
import qualified Persist.Sqlite as Lite
import Data.Pool (Pool, destroyAllResources)
import Database.Persist.Sqlite (SqlBackend)
import qualified User as User
import Manifest
import qualified Data.SemVer as SemVer
import qualified Data.HashMap.Strict as HM
import System.Directory
import System.FilePath

registerDummy :: Pool SqlBackend -> IO Bool
registerDummy pool = registerUser pool "creek@gmail.com" "hunter2"

upload :: IO (W.Response LByteString)
upload = W.postWith opts "http://localhost:3000/package" (W.partFile "file" "manifest.toml")
  where
    opts = W.defaults & W.auth ?~ W.basicAuth "creek@gmail.com" "hunter2"

listPackages :: IO (W.Response LByteString)
listPackages = W.get "http://localhost:3000/package"

createIndex :: IO (W.Response LByteString)
createIndex = W.get "http://localhost:3000/createIndex"

getIndex :: IO ()
getIndex = do
  r <- W.get "http://localhost:3000/index"
  let body = r ^. W.responseBody
  Tar.unpack "result" (Tar.read body)

getPool :: IO (Pool SqlBackend)
getPool = Lite.defaultSqlitePool

migrate :: Pool SqlBackend -> IO ()
migrate = Lite.migrateSqliteDatabase

registerUser :: Pool SqlBackend -> Text -> Text -> IO Bool
registerUser pool email pw = Lite.runDB pool (User.registerUser email (toS pw))

checkUser :: Pool SqlBackend -> Text -> Text -> IO (Maybe Bool)
checkUser pool email pw = Lite.runDB pool (User.checkUser email (toS pw))

free :: Pool SqlBackend -> IO ()
free = destroyAllResources

forK :: (Foldable t, Monad m) => m (t a) -> (a -> m b) -> m ()
forK xs f = do
  traverse_ f =<< xs

convertAllBowers :: IO ()
convertAllBowers = do
  forK (listDirectory "purescript") $ \package ->
    forK (listDirectory ("purescript" </> package)) $ \version -> do
      convertBowerToToml ("purescript" </> package </> version </> "bower.json") (toS (drop 1 version))
      renameFile
        ("purescript" </> package </> version </> "bower.json.toml")
        ("purescript" </> package </> version </> "manifest.toml")

convertBowerToToml :: FilePath -> Text -> IO ()
convertBowerToToml fp version' = do
  bower <- readFile fp
  let decoded = decodeManifest =<< note "wat" (A.decode (toS bower))
  case decoded of
    Right m -> writeFile (fp <> ".toml") (prettyPrintManifest m)
    Left err -> error (toS err)
  where
    decodeManifest =
      A.parseEither $ A.withObject "Manifest" $ \package -> do
        name <- package .: "name"
        version <- either (const mzero) pure . SemVer.fromText =<< pure version'
        deps <- package .:? "dependencies" .!= HM.empty
        pure (Manifest name version ">= 0.11.5" ["author"] (HM.toList deps))
