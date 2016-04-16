module Gobstones where

import qualified Text.Parsec.Token as P
import Text.Parsec
import Text.Parsec.Language( javaStyle )
import Language.Mulang
import Language.Mulang.Builder
import Data.Either
import Control.Applicative hiding ((<|>), many, optional)

gobstonesStyle = javaStyle

lexer = P.makeTokenParser gobstonesStyle
parens = P.parens lexer
reserved = P.reserved lexer
commaSep = P.commaSep lexer
braces = P.braces lexer

gbs :: String -> Expression
gbs string 
  | (Right v) <- parseGobstones string = v
  | (Left v) <- parseGobstones string = error (show v)

parseGobstones :: String -> Either ParseError Expression
parseGobstones = fmap compact . parse program ""

program :: Parsec String a [Expression]
program = many $ procedure <|> gbsProgram <|> function

gbsProgram = do
                reserved "program"
                c <- commands
                return $ ProgramDeclaration (unguardedBody [] c)

procedure = do
              reserved "procedure"
              (name, p) <- body upperIdentifier
              return $ ProcedureDeclaration name (unguardedBody p MuNull)

function = do
              reserved "function"
              (name, p) <- body lowerIdentifier
              return $ FunctionDeclaration name (unguardedBody p MuNull)

body identifier = do 
                    name <- identifier
                    p <- parameters
                    commands
                    return (name, p)

upperIdentifier = many1 letter
lowerIdentifier = upperIdentifier

commands = do
              c <- braces (many simpleCommand)
              return $ if null c then MuNull else Sequence c

simpleCommand = moverCall

moverCall = do
              reserved "Mover"
              d <- parens direction
              return $ (Application (Variable "Mover") [d])

direction = concreteDir "Norte" <|> concreteDir "Este" <|> concreteDir "Sur" <|> concreteDir "Oeste"
              
concreteDir name = do
                      reserved name
                      return (MuLiteral name)

arguments = do
              args <- parens $ commaSep lowerIdentifier
              return $ map Variable args

parameters = do
              args <- parens $ commaSep lowerIdentifier
              return $ map VariablePattern args