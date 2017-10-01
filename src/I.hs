module I where

-- GHCI helpers

import           Protolude

import qualified Codec.Archive.Tar as Tar
import           Control.Concurrent.Async
import           Control.Lens ((^.), (?~))
import           Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as HM
import           Data.Pool (Pool, destroyAllResources)
import qualified Data.SemVer as SemVer
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Database.Persist.Sqlite (SqlBackend)
import qualified GitHub as GH
import qualified GitHub.Endpoints.Repos as GH
import qualified GitHub.Endpoints.Repos.Contents as GH
import           Manifest
import qualified Network.Wreq as W
import qualified Persist.Sqlite as Lite
import           System.Directory
import           System.FilePath
import qualified User as User

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
  forK (listDirectory "bowerfiles") $ \package -> do
    forK (listDirectory ("bowerfiles" </> package)) $ \version -> do
      -- traceShowM (package, version)
      when (isRight (SemVer.fromText (toS (drop 1 version)))) $ do
        whenM (convertBowerToToml ("bowerfiles" </> package </> version </> "bower.json") (toS (drop 1 version))) $
          renameFile
            ("bowerfiles" </> package </> version </> "bower.json.toml")
            ("bowerfiles" </> package </> version </> "manifest.toml")

convertBowerToToml :: FilePath -> Text -> IO Bool
convertBowerToToml fp version' = do
  bower <- readFile fp
  let decoded = decodeManifest =<< note "Invalid json" (A.decode (toS bower))
  case decoded of
    Right m -> writeFile (fp <> ".toml") (prettyPrintManifest m) $> True
    Left err -> putText ("Failed to decode manifest: " <> show err) $> False
  where
    decodeManifest =
      A.parseEither $ A.withObject "Manifest" $ \package -> do
        name <- package .: "name"
        version <- either (const mzero) pure . SemVer.fromText =<< pure version'
        deps <- package .:? "dependencies" .!= HM.empty
        pure (Manifest name version ">= 0.11.5" ["author"] (HM.toList deps))

-- | Downloads all bower files for the repositories specified in `repos`. You
-- | have to provide a valid GitHub access token for `oauth`, and add the
-- | additional repos you want to download to the `repos` value.
downloadZeBowerFiles :: IO ()
downloadZeBowerFiles =
  for_ repos $ \repo -> do
    putText ("Downloading tags for: " <> show repo)
    tags <- fetchTags repo
    forConcurrently_ tags $ \tag ->
      unlessM (doesDirectoryExist (mkBowerPath (snd repo) tag)) $ do
        putText ("Downloading bower file for: " <> show repo <> show (GH.tagName tag))
        bowerJson <- downloadBowerJson repo tag
        case bowerJson of
          Just bj -> writeBowerJson repo tag (decodeContent bj)
          Nothing -> pure ()

---------------
-- | Customize these:
---------------
oauth :: GH.Auth
oauth = GH.OAuth ""

repos :: [(GH.Name GH.Owner, GH.Name GH.Repo)]
repos = map (bimap mkOwner (mkRepo . (<>) "purescript-"))
  [ ("purescript", "quickcheck")
  , ("purescript", "foreign")
  , ("purescript", "datetime")
  , ("purescript", "transformers")
  , ("purescript", "generics")
  , ("purescript", "maps")
  , ("purescript", "lazy")
  , ("purescript", "maybe")
  , ("purescript", "either")
  , ("purescript", "tuples")
  , ("purescript", "arrays")
  , ("purescript", "control")
  , ("purescript", "strings")
  , ("purescript", "foldable-traversable")
  , ("purescript", "math")
  , ("purescript", "monoid")
  , ("purescript", "globals")
  , ("purescript", "enums")
  , ("purescript", "random")
  , ("purescript", "exceptions")
  , ("purescript", "refs")
  , ("purescript", "free")
  , ("purescript", "validation")
  , ("purescript", "exists")
  , ("purescript", "bifunctors")
  , ("purescript", "lists")
  , ("purescript", "unfoldable")
  , ("purescript", "profunctor")
  , ("purescript", "distributive")
  , ("purescript", "contravariant")
  , ("purescript", "const")
  , ("purescript", "graphs")
  , ("purescript", "sets")
  , ("purescript", "identity")
  , ("purescript", "parallel")
  , ("purescript", "inject")
  , ("purescript", "tailrec")
  , ("purescript", "integers")
  , ("purescript", "invariant")
  , ("purescript", "semirings")
  , ("purescript", "proxy")
  , ("purescript", "eff")
  , ("purescript", "prelude")
  , ("purescript", "st")
  , ("purescript", "functions")
  , ("purescript", "console")
  , ("purescript", "assert")
  , ("purescript", "unsafe-coerce")
  , ("purescript", "nonempty")
  , ("purescript", "catenable-lists")
  , ("purescript", "functors")
  , ("purescript", "orders")
  , ("purescript", "partial")
  , ("purescript", "psci-support")
  , ("purescript", "newtype")
  , ("purescript", "symbols")
  , ("purescript", "generics-rep")
  , ("purescript", "type-equality")
  , ("purescript", "typelevel-prelude")
  , ("purescript", "gen")
  , ("purescript", "minibench")
  , ("purescript", "lcg")
  , ("purescript", "record")
  ]

bowerFileDirectory :: FilePath
bowerFileDirectory = "bowerfiles"
---------------
-- | End Customize
---------------

mkOwner :: Text -> GH.Name GH.Owner
mkOwner = GH.mkName (Proxy :: Proxy GH.Owner)

mkRepo :: Text -> GH.Name GH.Repo
mkRepo = GH.mkName (Proxy :: Proxy GH.Repo)

fetchTags :: (GH.Name GH.Owner, GH.Name GH.Repo) -> IO (V.Vector GH.Tag)
fetchTags (owner, repo) = do
  tags <- GH.tagsFor' (Just oauth) owner repo
  case tags of
    Left err -> panic ("Failed to fetch tags for: " <> GH.untagName owner <> "/" <> GH.untagName repo <> " with: " <> show err)
    Right tags' -> pure tags'

downloadBowerJson :: (GH.Name GH.Owner, GH.Name GH.Repo) -> GH.Tag -> IO (Maybe GH.ContentFileData)
downloadBowerJson (owner, repo) tag = do
  content <- GH.contentsFor' (Just oauth) owner repo "bower.json" (Just (GH.tagName tag))
  case content of
    Left err -> do
      putText ("Failed to bower.json for: " <> GH.untagName owner <> "/" <> GH.untagName repo <> ":" <> GH.tagName tag <> " with: " <> show err)
      pure Nothing
    Right (GH.ContentFile content') -> pure (Just content')
    _ -> panic "wot? bower.json was a directory"

decodeContent :: GH.ContentFileData -> Text
decodeContent fileContent =
  let decodeBase64 = TE.decodeUtf8 . Base64.decodeLenient . TE.encodeUtf8
  in decodeBase64 (GH.contentFileContent fileContent)

writeBowerJson :: (GH.Name GH.Owner, GH.Name GH.Repo) -> GH.Tag -> Text -> IO ()
writeBowerJson (_, repo) tag content = do
  let dirName = mkBowerPath repo tag
  createDirectoryIfMissing True dirName
  writeFile (dirName </> "bower.json") content

mkBowerPath :: GH.Name GH.Repo -> GH.Tag -> FilePath
mkBowerPath repo tag = bowerFileDirectory  </> toS (GH.untagName repo) </> toS (GH.tagName tag)
