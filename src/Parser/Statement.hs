module Parser.Statement (module Parser.Statement) where

import AST
import Control.Monad (void)
import Parser.Assignment
import Parser.EndOfLine
import Parser.Space
import Parser.Variable
import Text.Parsec
import Text.Parsec.String

parseStatement :: Parser Statement
parseStatement =
  ( (VariableStatement <$> try (spaces' *> (try parseVariable)))
      <|> (AssignmentStatement <$> try (spaces' *> (try parseAssignment)))
  )
    <* endOfStatement

endOfStatement :: Parser ()
endOfStatement = void (char ';') <|> eol
