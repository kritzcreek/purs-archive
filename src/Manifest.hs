{-# LANGUAGE RecordWildCards #-}
module Manifest where

import Protolude
import Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import qualified Text.Toml as Toml
import qualified Data.HashMap.Strict as HM

-- TODO: LOL
type Version = Text
type VersionRange = Text
type PackageName = Text
type Author = Text

data Manifest = Manifest
  { manifestName :: PackageName
  , manifestVersion :: Version
  , manifestAuthors :: [Author]
  , manifestDependencies :: [(PackageName, VersionRange)]
  } deriving (Show)

parseManifest :: Text -> Maybe Manifest
parseManifest = go . Aeson.toJSON <=< hush . Toml.parseTomlDoc "wot"
  where
    go :: Aeson.Value -> Maybe Manifest
    go =
      parseMaybe $ withObject "Manifest" $ \o -> do
        package <- o .: "package"
        name <- package .: "name"
        authors <- package .: "authors"
        version <- package .: "version"
        deps <- o .: "dependencies"
        pure (Manifest name version authors (HM.toList deps))

prettyPrintManifest :: Manifest -> Text
prettyPrintManifest Manifest{..} =
  Text.unlines $
    [ "[package]"
    , "name = " <> show manifestName
    , "version = " <> show manifestVersion
    , "authors = [" <> Text.intercalate ", " (map show manifestAuthors) <> "]"
    , ""
    , "[dependencies]"
    ] <> dependencies
    where
      dependencies = foreach manifestDependencies $ \(pn, vr) ->
        pn <> " = " <> show vr
