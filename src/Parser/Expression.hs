module Parser.Expression (module Parser.Expression) where

import AST
import Parser.Atomic
import Parser.Literal (parseLiteral)
import Parser.Operator (parseOperator)
import Parser.Space
import Parser.Type
import Text.Parsec
import Text.Parsec.String

parseExpression :: Parser Expression
parseExpression =
  try parseOperation
    <|> try parseAtomicExpression

parseOperation :: Parser Expression
parseOperation =
  do
    left <- parseAtomicExpression -- left side has to be atomic to avoid endless loop becase of left recursion
    _ <- spaces'
    op <- parseOperator
    _ <- spaces'
    OperationExpression left op <$> parseExpression

parseAtomicExpression :: Parser Expression
parseAtomicExpression = do
  AtomicExpression <$> parseAtomic
