module GobstonesSpec (spec) where

import           Test.Hspec
import           Gobstones
import           Language.Mulang

spec :: Spec
spec = do
  describe "parse" $ do
    it "empty program" $ do
      gbs "program {}" `shouldBe` ProgramDeclaration []

    it "empty procedure without parameters" $ do
      gbs "procedure SomeName() {}" `shouldBe` ProcedureDeclaration "SomeName" [Equation [] (UnguardedBody MuNull)]

    it "empty procedure with parameters" $ do
      gbs "procedure AnotherName(one, another) {}" `shouldBe` ProcedureDeclaration "AnotherName" [Equation [VariablePattern "one", VariablePattern "another"] (UnguardedBody MuNull)]

    it "empty function with parameters" $ do
      gbs "function computeSomething(number) {}" `shouldBe` FunctionDeclaration "computeSomething" [Equation [VariablePattern "number"] (UnguardedBody MuNull)]      