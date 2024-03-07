module Parser.Type (module Parser.Type) where

import AST
import Text.Parsec
import Text.Parsec.String

parseType :: Parser Type
parseType =
  (string "void" >> return UnitType)
    <|> (string "int" >> return IntType)
    <|> (string "float" >> return FloatType)
    <|> (string "bool" >> return BoolType)
    <|> (CustomType <$> parseVariableName)

parseVariableName :: Parser Name
parseVariableName = do
  many1 letter
