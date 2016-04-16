module GobstonesSpec (spec) where

import           Test.Hspec
import           Gobstones
import           Language.Mulang
import           Language.Mulang.Builder

spec :: Spec
spec = do
  describe "parse" $ do
    it "empty program" $ do
      gbs "program {}" `shouldBe` ProgramDeclaration (unguardedBody [] MuNull)

    it "program with one command" $ do
      gbs "program { Mover(Norte) }" `shouldBe` ProgramDeclaration (unguardedBody [] $ Sequence [Application (Variable "Mover") [MuLiteral "Norte"]])

    it "empty procedure without parameters" $ do
      gbs "procedure SomeName() {}" `shouldBe` ProcedureDeclaration "SomeName" (unguardedBody [] MuNull)

    it "empty procedure with parameters" $ do
      gbs "procedure AnotherName(one, another) {}" `shouldBe` ProcedureDeclaration "AnotherName" (unguardedBody [VariablePattern "one", VariablePattern "another"] MuNull)

    it "empty function with parameters" $ do
      gbs "function computeSomething(number) {}" `shouldBe` FunctionDeclaration "computeSomething" (unguardedBody [VariablePattern "number"] MuNull) 