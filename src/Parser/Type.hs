module Parser.Type (module Parser.Type) where

import AST
import Parser.Name
import Text.Parsec
import Text.Parsec.String

parseType :: Parser Type
parseType =
  try (string "void" >> return UnitType)
    <|> try (string "int" >> return IntType)
    <|> try (string "float" >> return FloatType)
    <|> try (string "bool" >> return BoolType)
    <|> try (string "str" >> return StringType)
    <|> (CustomType <$> parseName)
