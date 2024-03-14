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
  ControlStatement <$> try (spaces' *> parseControl)
    <|> try parseReturnStatement
    <|> FunctionDefinitionStatement <$> try (spaces' *> parseFunction)
    <|> VariableStatement <$> try (spaces' *> parseVariable) <* endOfStatement
    <|> AssignmentStatement <$> try (spaces' *> parseAssignment) <* endOfStatement
    <|> ExpressionStatement <$> try (spaces' *> parseExpression) <* endOfStatement

parseReturnStatement :: Parser Statement
parseReturnStatement = do
  _ <- spaces'
  _ <- string "return"
  expr <- optionMaybe (spaces1' *> parseExpression)
  _ <- endOfStatement
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
  vars <- parseVariableDeclaration `sepBy` (spaces' *> char ',' <* spaces')
  _ <- char ')'
  _ <- spaces'
  _ <- char '{'
  _ <- spaces'
  statements <- manyTill (parseStatement <* spaces') (spaces' *> char '}' <* spaces')
  return $ Function name vars fnType statements

parseControl :: Parser Control
parseControl =
  parseIfControl <|> parseWhileControl

parseIfControl :: Parser Control
parseIfControl = do
  _ <- spaces'
  _ <- string "if"
  test <- spaces1' *> parseExpression
  _ <- spaces1' *> char '{'
  trueBlock <- spaces' *> many parseStatement
  _ <- spaces' *> char '}'
  elseBlock <- optionMaybe (spaces' *> string "else" *> spaces' *> char '{' *> spaces' *> many parseStatement <* spaces' <* char '}')
  case elseBlock of
    Nothing -> return $ IfControl test trueBlock Nothing
    Just els -> return $ IfControl test trueBlock (Just els)

parseWhileControl :: Parser Control
parseWhileControl = do
  _ <- spaces'
  _ <- string "while"
  test <- spaces1' *> parseExpression
  _ <- spaces1' *> char '{'
  block <- spaces' *> many parseStatement
  _ <- spaces' *> char '}'
  return $ WhileControl test block
