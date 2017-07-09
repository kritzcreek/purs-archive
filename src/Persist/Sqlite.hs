module Persist.Sqlite where

import Protolude

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool (Pool)
import Database.Persist.Sqlite
import Persist (migrateAll)

makeSqlitePool :: Text -> Int -> IO (Pool SqlBackend)
makeSqlitePool dbName poolSize =
  runNoLoggingT $ createSqlitePool dbName poolSize

defaultSqlitePool :: IO (Pool SqlBackend)
defaultSqlitePool =
  makeSqlitePool "data.sqlite3" 10

migrateSqliteDatabase :: Pool SqlBackend -> IO ()
migrateSqliteDatabase pool =
  runNoLoggingT (runSqlPool (runMigration migrateAll) pool)

runDB :: Pool SqlBackend -> SqlPersistM a -> IO a
runDB pool action = do
  runResourceT $ runNoLoggingT $ runSqlPool action pool
