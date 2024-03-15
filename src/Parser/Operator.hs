module Parser.Operator (module Parser.Operator) where

import AST
import Text.Parsec
import Text.Parsec.String

parseOperator :: Parser Operator
parseOperator =
  try (string "+" >> return Plus)
    <|> try (string "<=" >> return Le)
    <|> try (string ">=" >> return Ge)
    <|> try (string "-" >> return Minus)
    <|> try (string "*" >> return Multiply)
    <|> try (string "/" >> return Divide)
    <|> try (string "%" >> return Modulus)
    <|> try (string "&&" >> return And)
    <|> try (string "||" >> return Or)
    <|> try (string "==" >> return Eq)
    <|> try (string "!=" >> return Neq)
    <|> try (string "!" >> return Not)
    <|> try (string "<" >> return Lt)
    <|> try (string ">" >> return Gt)
