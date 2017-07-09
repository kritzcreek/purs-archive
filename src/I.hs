module I where

-- GHCI helpers

import Protolude hiding (get, put)
import Control.Lens ((^.), (?~))
import qualified Codec.Archive.Tar as Tar
import qualified Network.Wreq as W

import qualified Persist.Sqlite as Lite
import Data.Pool (Pool, destroyAllResources)
import Database.Persist.Sqlite (SqlBackend)
import qualified User as User

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
