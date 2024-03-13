module Parser.Name (module Parser.Name) where

import AST
import Text.Parsec
import Text.Parsec.String

parseName :: Parser Name
parseName = do
  fistChar <- startChar
  rest <- many (digit <|> startChar)
  return (fistChar : rest)
  where
    startChar = letter <|> char '_'
