module Parser.Expression (module Parser.Expression) where

import AST
import Parser.Literal (parseLiteral)
import Parser.Operator (parseOperator)
import Parser.Space
import Parser.Type
import Text.Parsec
import Text.Parsec.String

parseExpression :: Parser Expression
parseExpression =
  try parseOperation
    <|> try parseAtomic

parseOperation :: Parser Expression
parseOperation =
  do
    left <- parseAtomic -- left side has to be atomic to avoid endless loop becase of left recursion
    _ <- spaces'
    op <- parseOperator
    _ <- spaces'
    right <- parseExpression
    return $ OperationExpression left op right

parseAtomic :: Parser Expression
parseAtomic = do
  AtomicExpression <$> parseAtomic'
  where
    parseAtomic' =
      LiteralAtomic <$> parseLiteral
        <|> VariableAtomic <$> parseVariableName
