{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (fileContent)
import Archive (persistManifest, listPackages, createIndex)
import qualified Network.HTTP.Types as HTTP
import Manifest

main :: IO ()
main = Scotty.scotty 3000 $ do
  Scotty.middleware logStdoutDev
  Scotty.get "/package" $ do
    Scotty.json =<< liftIO listPackages
  Scotty.post "/package" $ do
    fs <- Scotty.files
    let content = fileContent . snd <$> head fs
    case parseManifest . toS =<< content of
      Nothing ->
        Scotty.status HTTP.badRequest400
      Just manifest ->
        liftIO (persistManifest manifest)
  Scotty.get "/createIndex" (liftIO createIndex)
  Scotty.get "/index" (Scotty.file "index.tar")
