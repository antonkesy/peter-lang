module Parser.Atomic (module Parser.Atomic) where

import AST
import Parser.Literal (parseLiteral)
import Parser.Space
import Parser.Type
import Text.Parsec
import Text.Parsec.String

parseAtomic :: Parser Atomic
parseAtomic =
  LiteralAtomic
    <$> parseLiteral
    <|> VariableAtomic
      <$> parseVariableName
    <|> parseFunctionCallAtomic

parseFunctionCallAtomic :: Parser Atomic
parseFunctionCallAtomic = do
  name <- parseVariableName
  _ <- char '('
  args <- parseAtomic `sepBy` (spaces' >> char ',' >> spaces')
  _ <- char ')'
  return $ FunctionCallAtomic name args
