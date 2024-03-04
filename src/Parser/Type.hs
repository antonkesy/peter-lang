module Parser.Type (module Parser.Type) where

import AST
import Text.Parsec
import Text.Parsec.String

parseType :: Parser Type
parseType =
  (string "()" >> return UnitType)
    <|> (string "int" >> return IntType)
    <|> (string "float" >> return FloatType)
    <|> (string "bool" >> return BoolType)
    <|> (CustomType <$> (parseVariableName))

parseVariableName :: Parser Name
parseVariableName = do
  name <- many1 letter
  return name
