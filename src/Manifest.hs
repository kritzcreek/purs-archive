module Manifest where

import Protolude
import Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Text.Toml as Toml
import qualified Data.HashMap.Strict as HM

-- TODO: LOL
type Version = Text
type VersionRange = Text
type PackageName = Text

data Manifest = Manifest
  { manifestName :: PackageName
  , manifestVersion :: Version
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
        version <- package .: "version"
        deps <- o .: "dependencies"
        pure (Manifest name version (HM.toList deps))
