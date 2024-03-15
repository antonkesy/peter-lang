module Parser.Struct (module Parser.Struct) where

import AST
import Parser.Name
import Parser.Space
import Parser.Variable
import Text.Parsec
import Text.Parsec.String

parseStruct :: Parser Struct
parseStruct = do
  _ <- string "struct"
  _ <- spaces1'
  name <- parseName
  _ <- spaces'
  _ <- char '{'
  _ <- spaces'
  fields <- parseVariableDeclaration `sepEndBy` (spaces' *> char ';' <* spaces')
  _ <- spaces'
  _ <- char '}'
  return $ Struct name fields
