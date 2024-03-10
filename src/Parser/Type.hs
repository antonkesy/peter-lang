module Parser.Type (module Parser.Type) where

import AST
import Parser.Name
import Text.Parsec
import Text.Parsec.String

parseType :: Parser Type
parseType =
  (string "void" >> return UnitType)
    <|> (string "int" >> return IntType)
    <|> (string "float" >> return FloatType)
    <|> (string "bool" >> return BoolType)
    <|> (string "str" >> return StringType)
    <|> (CustomType <$> parseName)
