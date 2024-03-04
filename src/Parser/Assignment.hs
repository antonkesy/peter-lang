module Parser.Assignment (module Parser.Assignment) where

import AST
import Parser.Expression
import Parser.Space
import Parser.Type
import Text.Parsec
import Text.Parsec.String

parseAssignment :: Parser Assignment
parseAssignment =
  do
    var <- parseVariableName
    _ <- spaces'
    _ <- char '='
    _ <- spaces'
    expr <- parseExpression
    return (Assignment var expr)
