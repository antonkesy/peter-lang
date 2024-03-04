module Parser.Variable (module Parser.Variable) where

import AST
import Parser.Expression
import Parser.Space
import Parser.Type
import Text.Parsec
import Text.Parsec.String

parseVariable :: Parser Variable
parseVariable = do
  varType <- parseType
  _ <- spaces'
  name <- parseVariableName
  _ <- spaces'
  _ <- char '='
  _ <- spaces'
  expr <- parseExpression
  return $ Variable name varType expr
