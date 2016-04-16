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
braces = P.braces lexer
commaSep = P.commaSep lexer
reserved = P.reserved lexer

gbs :: String -> Expression
gbs string 
  | (Right v) <- parseGobstones string = v
  | (Left v) <- parseGobstones string = error (show v)

parseGobstones :: String -> Either ParseError Expression
parseGobstones = fmap compact . parse program ""

program :: Parsec String a [Expression]
program = many $ (try procedure) <|> gbsProgram

gbsProgram = do
                reserved "program"
                commands
                return $ ProgramDeclaration []

procedure = do
              reserved "procedure"
              name <- upperIdentifier
              parameters
              commands
              return $ ProcedureDeclaration name []

upperIdentifier = many1 letter
lowerIdentifier = upperIdentifier

commands = spaces <* char '{' <* spaces <* char '}'

parameters = do
              char '('
              args <- commaSep lowerIdentifier
              char ')'
              return $ map VariablePattern args