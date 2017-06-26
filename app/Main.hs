{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (fileContent)
import Archive
import Manifest

main :: IO ()
main = Scotty.scotty 3000 $ do
  Scotty.middleware logStdoutDev
  Scotty.get "/:word" $ do
    beam <- Scotty.param "word"
    Scotty.html (toS $ fold ["<h1>Scotty, ", beam, " me up! ", lol, "</h1>"])
  Scotty.post "/upload" $ do
    fs <- Scotty.files
    traceShowM (parseManifest . toS =<< fileContent . snd <$> (head fs ))
