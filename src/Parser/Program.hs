module Parser.Program (module Parser.Program) where

import AST
import Parser.Space
import Parser.Statement
import Text.Parsec
import Text.Parsec.String

-- TODO: rename to File
parseProgram :: Parser Program
parseProgram = do
  statements <- manyTill (spaces' *> parseStatement <* spaces') (try (spaces' *> eof))
  return $ Program statements
