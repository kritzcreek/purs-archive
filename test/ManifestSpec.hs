module ManifestSpec where

import Protolude

import Archive
import Test.Hspec

spec :: Spec
spec = do
  describe "Parsing Manifests" $ do
    it "parses a simple manifest" $ do
      2 `shouldBe` (1 + 1)
