module Parser.Variable (module Parser.Variable) where

import AST
import Parser.Expression
import Parser.Space
import Parser.Type
import Text.Parsec
import Text.Parsec.String

parseVariableDeclaration :: Parser VariableDeclaration
parseVariableDeclaration = do
  varType <- parseType
  _ <- spaces'
  name <- parseVariableName
  return $ VariableDeclaration name varType

parseVariable :: Parser Variable
parseVariable = do
  decl <- parseVariableDeclaration
  _ <- spaces'
  _ <- char '='
  _ <- spaces'
  Variable decl <$> parseExpression
