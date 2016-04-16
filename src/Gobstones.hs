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
                commands
                return $ ProgramDeclaration []

procedure = do
              reserved "procedure"
              name <- upperIdentifier
              p <- parameters
              commands
              return $ ProcedureDeclaration name [Equation p (UnguardedBody MuNull)]

function = do
              reserved "function"
              name <- lowerIdentifier
              p <- parameters
              commands
              return $ FunctionDeclaration name [Equation p (UnguardedBody MuNull)]              

upperIdentifier = many1 letter
lowerIdentifier = upperIdentifier

commands = braces spaces

parameters = do
              args <- parens $ commaSep lowerIdentifier
              return $ map VariablePattern args