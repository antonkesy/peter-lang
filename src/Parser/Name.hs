module Parser.Name (module Parser.Name) where

import AST
import Text.Parsec
import Text.Parsec.String

parseName :: Parser Name
parseName = do
  fistChar <- letter
  rest <- many (digit <|> letter)
  return (fistChar : rest)
