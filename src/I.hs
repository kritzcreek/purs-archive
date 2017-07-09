module I where

-- GHCI helpers

import Protolude hiding (get, put)
import Control.Lens ((^.))
import qualified Codec.Archive.Tar as Tar
import qualified Network.Wreq as W

import Persist.Sqlite
import Data.Pool (Pool)
import Database.Persist.Sqlite
import qualified User as User

upload :: IO (W.Response LByteString)
upload = W.post "http://localhost:3000/package" (W.partFile "file" "manifest.toml")

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
getPool = defaultSqlitePool

registerUser :: Pool SqlBackend -> Text -> Text -> IO ()
registerUser pool email pw = runDB pool (User.registerUser email (toS pw))

checkUser :: Pool SqlBackend -> Text -> Text -> IO (Maybe Bool)
checkUser pool email pw = runDB pool (User.checkUser email (toS pw))

