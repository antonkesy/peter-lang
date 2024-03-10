module Parser.Assignment (module Parser.Assignment) where

import AST
import Parser.Expression
import Parser.Name
import Parser.Space
import Text.Parsec
import Text.Parsec.String

parseAssignment :: Parser Assignment
parseAssignment =
  do
    var <- parseName
    _ <- spaces'
    _ <- char '='
    _ <- spaces'
    Assignment var <$> parseExpression
