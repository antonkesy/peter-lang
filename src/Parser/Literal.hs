module Parser.Literal (module Parser.Literal) where

import AST
import Text.Parsec
import Text.Parsec.String

parseLiteral :: Parser Literal
parseLiteral =
  try parseUnitLiteral
    <|> try parseBoolLiteral
    <|> try parseFloatLiteral
    <|> try parseIntLiteral

parseIntLiteral :: Parser Literal
parseIntLiteral = do
  sign <- optionMaybe (char '-')
  n <- many1 digit
  return $ case sign of
    Just _ -> IntLiteral (read ('-' : n))
    Nothing -> IntLiteral (read n)

parseFloatLiteral :: Parser Literal
parseFloatLiteral = do
  sign <- optionMaybe (char '-')
  n <- many1 digit
  _ <- char '.'
  m <- many1 digit
  return $ case sign of
    Just _ -> FloatLiteral (read ('-' : n ++ "." ++ m))
    Nothing -> FloatLiteral (read (n ++ "." ++ m))

parseBoolLiteral :: Parser Literal
parseBoolLiteral =
  (string "true" >> return (BoolLiteral True))
    <|> (string "false" >> return (BoolLiteral False))

parseUnitLiteral :: Parser Literal
parseUnitLiteral = string "()" >> return UnitLiteral
