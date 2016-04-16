module Gobstones where

import Text.Parsec
import Language.Mulang
import Language.Mulang.Builder
import Data.Either

gbs :: String -> Expression
gbs string | (Right v) <- parseGobstones string = v

parseGobstones :: String -> Either ParseError Expression
parseGobstones = fmap compact . parse program ""

program :: Parsec String a [Expression]
program = many gbsProgram

gbsProgram = do
                string "program"
                spaces
                char '{'
                spaces
                char '}'
                return $ ProgramDeclaration []