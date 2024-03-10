module Parser.Statement (module Parser.Statement) where

import AST
import Control.Monad (void)
import Parser.Assignment
import Parser.EndOfLine
import Parser.Expression
import Parser.Name
import Parser.Space
import Parser.Type
import Parser.Variable
import Text.Parsec
import Text.Parsec.String

parseStatement :: Parser Statement
parseStatement =
  try parseReturnStatement
    <|> (FunctionDefinitionStatement <$> try (spaces' *> try parseFunction))
    <|> (VariableStatement <$> try (spaces' *> try parseVariable) <* endOfStatement)
    <|> (AssignmentStatement <$> try (spaces' *> try parseAssignment) <* endOfStatement)
    <|> (ExpressionStatement <$> try (spaces' *> try parseExpression) <* endOfStatement)

parseReturnStatement :: Parser Statement
parseReturnStatement = do
  _ <- spaces'
  _ <- try (string "return")
  expr <- try (optionMaybe (spaces1' *> parseExpression))
  _ <- try endOfStatement
  case expr of
    Nothing -> return $ ReturnStatement (AtomicExpression (LiteralAtomic UnitLiteral))
    Just ex -> return $ ReturnStatement ex

endOfStatement :: Parser ()
endOfStatement = void (char ';') <|> eol

parseFunction :: Parser Function
parseFunction = do
  fnType <- parseType
  _ <- spaces1'
  name <- parseName
  _ <- char '('
  vars <- try parseVariableDeclaration `sepBy` (spaces' *> char ',' <* spaces')
  _ <- char ')'
  _ <- spaces'
  _ <- char '{'
  _ <- spaces'
  statements <- manyTill (try parseStatement <* spaces') (spaces' *> char '}' <* spaces')
  return $ Function name vars fnType statements
