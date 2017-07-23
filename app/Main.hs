{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import Network.Wai.Parse (fileContent)
import Archive (persistManifest, listPackages, createIndex, PersistError(..))
import qualified Network.HTTP.Types as HTTP
import Manifest
import User (checkUser)
import Database.Persist.Sqlite (SqlBackend)
import qualified Persist.Sqlite as Lite
import Data.Pool (Pool)

main :: IO ()
main = do
  pool <- Lite.defaultSqlitePool
  app pool

app :: Pool SqlBackend -> IO ()
app pool = Scotty.scotty 3000 $ do
  Scotty.middleware logStdoutDev
  Scotty.get "/package" $ do
    Scotty.json =<< liftIO (Lite.runDB pool listPackages)
  Scotty.post "/package" $ do
    email <- checkAuth
    fs <- Scotty.files
    let content = fileContent . snd <$> head fs
    case parseManifest . toS =<< content of
      Nothing ->
        Scotty.status HTTP.badRequest400
      Just manifest -> do
        mErr <- liftIO (Lite.runDB pool (persistManifest (toS email) manifest))
        case mErr of
          Just PackageAlreadyTaken -> do
            Scotty.status HTTP.conflict409
            Scotty.text "Package name was already taken."
          Just VersionAlreadyUploaded -> do
            Scotty.status HTTP.conflict409
            Scotty.text "Version was already uploaded."
          _ -> pure ()
  Scotty.get "/createIndex" (liftIO createIndex)
  Scotty.get "/index" (Scotty.file "index.tar")
  where
    checkAuth = do
      auth <- Scotty.header "Authorization"
      case extractBasicAuth . toS =<< auth of
        Nothing ->
          Scotty.status HTTP.unauthorized401 *> Scotty.finish
        Just (email, pw) -> do
          authResult <- liftIO $ Lite.runDB pool (checkUser (toS email) pw)
          if fromMaybe False authResult
            then pure email
            else Scotty.status HTTP.unauthorized401 *> Scotty.finish
