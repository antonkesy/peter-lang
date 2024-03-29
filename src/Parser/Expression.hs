module Parser.Expression (module Parser.Expression) where

import AST
import Parser.Literal (parseLiteral)
import Parser.Name
import Parser.Operator (parseOperator)
import Parser.Space
import Text.Parsec
import Text.Parsec.String

parseExpression :: Parser Expression
parseExpression =
  try parseExpression'
    <|> try
      ( char '('
          *> spaces'
          *> parseExpression'
          <* spaces'
          <* char ')'
      )
  where
    parseExpression' = try parseOperation <|> try parseAtomicExpression

parseOperation :: Parser Expression
parseOperation =
  do
    left <- try parseAtomicExpression -- left side has to be atomic to avoid endless loop becase of left recursion
    _ <- spaces'
    op <- try parseOperator
    _ <- spaces'
    OperationExpression left op <$> try parseExpression

parseAtomicExpression :: Parser Expression
parseAtomicExpression =
  try parseAtomic'
    <|> try
      ( char '('
          *> spaces'
          *> parseAtomic'
          <* spaces'
          <* char ')'
      )
  where
    parseAtomic' = AtomicExpression <$> try parseAtomic

parseFunctionCallAtomic :: Parser Atomic
parseFunctionCallAtomic = do
  name <- try parseName
  _ <- spaces' *> char '(' <* spaces'
  args <- try ((spaces *> parseExpression <* spaces') `sepBy` (spaces' >> char ',' >> spaces'))
  _ <- spaces' *> char ')'
  return $ FunctionCallAtomic name args

parseAtomic :: Parser Atomic
parseAtomic =
  LiteralAtomic <$> try parseLiteral
    <|> try parseFunctionCallAtomic
    <|> VariableAtomic <$> try parseExistingVariableName
