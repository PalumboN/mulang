module GobstonesSpec (spec) where

import           Test.Hspec
import           Gobstones
import           Language.Mulang

spec :: Spec
spec = do
  describe "parse" $ do
    it "empty program" $ do
      gbs "program {}" `shouldBe` ProgramDeclaration []

    it "empty procedure" $ do
      gbs "procedure SomeName() {}" `shouldBe` ProcedureDeclaration "SomeName" []