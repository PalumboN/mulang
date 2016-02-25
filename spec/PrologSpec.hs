module PrologSpec (spec) where

import           Test.Hspec
import           Prolog
import           Language.Mulang

spec :: Spec
spec = do
  describe "parse" $ do
    it "simplest fact/0" $ do
      pl "foo." `shouldBe` [FactDeclaration "foo"]

    it "simplest fact/1" $ do
      pl "foo(bar)." `shouldBe` [FactDeclaration "foo"]

    it "simplest rule/1" $ do
      pl "baz(bar):-foo." `shouldBe` [RuleDeclaration "baz"]

    it "simplest rule/0" $ do
      pl "baz:-foo." `shouldBe` [RuleDeclaration "baz"]

    it "simplest rule/1 with condition/1" $ do
      pl "baz(bar):-foo(bar)." `shouldBe` [RuleDeclaration "baz"]
