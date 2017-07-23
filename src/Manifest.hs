{-# LANGUAGE RecordWildCards #-}
module Manifest where

import Protolude
import Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.SemVer (Version)
import qualified Data.SemVer as SemVer
import qualified Data.Text as Text
import qualified Text.Toml as Toml
import qualified Data.HashMap.Strict as HM

-- TODO: LOL
type VersionRange = Text
type PackageName = Text
type Author = Text

data Manifest = Manifest
  { manifestName :: PackageName
  , manifestVersion :: Version
  , manifestCompilerVersion :: VersionRange
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
        version <- either (const mzero) pure . SemVer.fromText =<< package .: "version"
        compiler <- package .: "compiler"
        deps <- o .:? "dependencies" .!= HM.empty
        pure (Manifest name version compiler authors (HM.toList deps))

prettyPrintManifest :: Manifest -> Text
prettyPrintManifest Manifest{..} =
  Text.unlines $
    [ "[package]"
    , "name = " <> show manifestName
    , "version = " <> "\"" <> SemVer.toText manifestVersion <> "\""
    , "compiler = " <> show manifestCompilerVersion
    , "authors = [" <> Text.intercalate ", " (map show manifestAuthors) <> "]"
    , ""
    , "[dependencies]"
    ] <> dependencies
    where
      dependencies = foreach manifestDependencies $ \(pn, vr) ->
        pn <> " = " <> show vr
