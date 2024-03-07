module Parser.Statement (module Parser.Statement) where

import AST
import Control.Monad (void)
import Parser.Assignment
import Parser.EndOfLine
import Parser.Expression
import Parser.Space
import Parser.Type
import Parser.Variable
import Text.Parsec
import Text.Parsec.String

parseStatement :: Parser Statement
parseStatement =
  ( (VariableStatement <$> try (spaces' *> try parseVariable))
      <|> (AssignmentStatement <$> try (spaces' *> try parseAssignment))
      <|> (FunctionDefinitionStatement <$> try (spaces' *> try parseFunction))
      <|> ExpressionStatement <$> try (spaces' *> try parseExpression)
  )
    <* spaces
    <* endOfStatement

endOfStatement :: Parser ()
endOfStatement = void (char ';') <|> eol

parseFunction :: Parser Function
parseFunction = do
  fnType <- parseType
  _ <- spaces'
  name <- parseFunctionName
  _ <- char '('
  vars <- parseVariableDeclaration `sepBy` (char ',' >> spaces')
  _ <- char ')'
  _ <- spaces'
  _ <- char '{'
  _ <- spaces'
  statements <- many parseStatement
  _ <- spaces'
  _ <- char '}'
  return $ Function name vars fnType statements

parseFunctionName :: Parser Name
parseFunctionName = do
  many1 letter
