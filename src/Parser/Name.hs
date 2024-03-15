module Parser.Name (module Parser.Name) where

import AST
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String

parseName :: Parser Name
parseName = do
  fistChar <- startChar
  rest <- many (digit <|> startChar)
  return (fistChar : rest)
  where
    startChar = letter <|> char '_'

parseMemberName :: Parser Name
parseMemberName = do
  firstHalf <- parseName
  _ <- char '.'
  rest <- parseName `sepBy1` string "."
  return (firstHalf ++ "." ++ intercalate "." rest)

parseExistingVariableName :: Parser Name
parseExistingVariableName =
  try parseMemberName <|> try parseName
