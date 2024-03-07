module Parser.Atomic (module Parser.Atomic) where

import AST
import Parser.Literal (parseLiteral)
import Parser.Space
import Parser.Type
import Text.Parsec
import Text.Parsec.String

parseAtomic :: Parser Atomic
parseAtomic =
  (LiteralAtomic <$> try parseLiteral)
    <|> try parseFunctionCallAtomic
    <|> (VariableAtomic <$> try parseVariableName)

parseFunctionCallAtomic :: Parser Atomic
parseFunctionCallAtomic = do
  name <- try parseVariableName
  _ <- char '('
  args <- try (parseAtomic `sepBy` (spaces' >> char ',' >> spaces'))
  _ <- char ')'
  return $ FunctionCallAtomic name args
