module DuplicationsInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.DuplicateCode

spec :: Spec
spec = do
  describe "hasDuplicateCode" $ do
    describe "with bodys Exactly equals" $ do
      it "is False when two procedures are not equals" $ do
        let code = "function F(){} function G(){Sacar(Verde)}"
        hasDuplicateCode (js code) `shouldBe` False

      it "is False when two functions have different return" $ do
        let code = "function F(){ return 4 } function G(){ return 45 }"
        hasDuplicateCode (js code) `shouldBe` False
      
      it "is False when two functions have same literal return " $ do
        let code = "function f(){ Sacar(Verde) return 2 } function g(){return 2 }"
        hasDuplicateCode (js code) `shouldBe` False

      it "is False when two functions have same expressions" $ do
        let code = "function f(){ Sacar(Verde) return x == y } function g(){ while(x==y){} return 2 }"
        hasDuplicateCode (js code) `shouldBe` False

      describe "is False when two computations have marginal similities" $ do
        it "different operations on same variable in return" $ do 
          hasDuplicateCode (js "function F(){return x + 1 } function G(){ return x - 1  }") `shouldBe` False

        it "only similar increments in body" $ do 
          hasDuplicateCode (js "function F(){ x = x + 1 ;  return x + 1 } function G(){ x = x + 1 ; return x - 1  }") `shouldBe` False

        it "only similar procedure calls in body" $ do 
          hasDuplicateCode (js "function F(){ Mover(Sur) ; Poner(Rojo) ; Mover(Norte) }\
                                \function G(){ Mover(Sur) ; Poner(Verde) ; Mover(Este) }") `shouldBe` False

      it "is False when two functions have different body" $ do
        let code = "function f(){ Sacar(Verde) return 2 } function g(){return 3 }"
        hasDuplicateCode (js code) `shouldBe` False

      it "is False when a function no repeat code" $ do
        let code = "function f(){ PonerYmover(Norte,Verde) PonerYmover(Este,Verde) PonerYmover(Sur,Verde) PonerYmover(Oeste,Verde) }"
        hasDuplicateCode (js code) `shouldBe` False

      it "is False when a function no repeat code" $ do
        let code = "function f(){ Poner(Rojo) Mover(Norte) return superF(Azul,Verde)} function g() {if(superF(Azul,Verde)){Poner(Rojo)}else{Poner(Verde)}}"
        hasDuplicateCode (js code) `shouldBe` False

      it "is False when a function no repeat code" $ do
        let code = "function f(){if(puedeMover(Este)){ MoverYPonerDiagonal(Este) } else{ MoverYPonerDiagonal(Oeste)  } }"
        hasDuplicateCode (js code) `shouldBe` False

      it "is False when a function no repeat code" $ do
        let code = "function f(){ Proc() return (hayBolitas(Verde) && puedeMover(Este))} function Proc(){Poner(Verde) Mover(Sur)}"
        hasDuplicateCode (js code) `shouldBe` False

      it "is False when a function no repeat code" $ do
        let code = "function f(){Poner(Verde) Sarasa() Sarasa()} function Sarasa(){Mover(Este) Mover(Este) }"
        hasDuplicateCode (js code) `shouldBe` False

      it "is False when a function has overlapped repeated code" $ do
        let code = "function f(){Poner(Verde) Sarasa() Sarasa()} function Sarasa(){Mover(Este) Mover(Este) Mover(Este) }"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when two functions are equals" $ do
        let code = "function f(){if(true){}else{} return x} function g(){if(true){}else{} return x}"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when a function and a procedure have same body" $ do
        let code = "function G(){if(true){}else{} } function f(){if(true){}else{} return x}"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when two functions have same body" $ do
        let code = "function f(){ Sacar(Verde) return 2 } function g(){ Sacar(Verde) return 2 }"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when two functions have same expressions" $ do
        let code = "function f(){ Sacar(Verde) return (x * 2) == y } function g(){ while((x * 2) == y){} return 2 }"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when a function repeat code" $ do
        let code = "function f(){Poner(Verde) Mover(Norte) Poner(Verde) Mover(Norte) Poner(Verde) Mover(Norte) Poner(Verde) Mover(Norte) }"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when a function repeat code" $ do
        let code = "function f(){Poner(Verde) MoverDestino(Norte,Este) Poner(Verde) MoverDestino(Norte,Este) Poner(Verde) VolverAlOrigen() Poner(Negro) MoverDestino(Norte,Este) Poner(Negro) MoverDestino(Norte,Este) Poner(Negro) }"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when a function repeat code" $ do
        let code = "function f(){if(puedeMover(Este)){Mover(Este) Poner(Rojo) Poner(Verde) Poner(Negro)} else{Mover(Oeste) Poner(Rojo) Poner(Verde) Poner(Negro)} }"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when a function repeat code" $ do
        let code = "function f(){if(puedeMover(Este)){ Mover(Este) PonerDiagonal() } else{ Mover(Este) PonerDiagonal() } }"
        hasDuplicateCode (js code) `shouldBe` True

      it "is True when a function repeat code" $ do
        let code = "function f(){ Poner(Rojo) Mover(Este) return (puedeMover(Este) && hayBolitas(Rojo) && hayBolitas(Verde))} function g(){if(puedeMover(Este) && hayBolitas(Rojo) && hayBolitas(Verde)){Poner(Rojo)}else{if(puedeMover(Este) && hayBolitas(Azul) && hayBolitas(Negro)){Poner(Verde)}else{} } }"
        hasDuplicateCode (js code) `shouldBe` True











