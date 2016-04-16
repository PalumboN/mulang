module Gobstones where

import Text.Parsec
import Language.Mulang
import Language.Mulang.Builder
import Data.Either
import Control.Applicative hiding ((<|>), many)

gbs :: String -> Expression
gbs string | (Right v) <- parseGobstones string = v

parseGobstones :: String -> Either ParseError Expression
parseGobstones = fmap compact . parse program ""

program :: Parsec String a [Expression]
program = many gbsProgram

commands = spaces <* char '{' <* spaces <* char '}'

gbsProgram = do
                string "program"
                commands
                return $ ProgramDeclaration []