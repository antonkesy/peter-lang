module Parser.Operator (module Parser.Operator) where

import AST
import Text.Parsec
import Text.Parsec.String

parseOperator :: Parser Operator
parseOperator =
  (string "+" >> return Plus)
    <|> (string "-" >> return Minus)
    <|> (string "*" >> return Multiply)
    <|> (string "/" >> return Divide)
    <|> (string "%" >> return Modulus)
    <|> (string "&&" >> return And)
    <|> (string "||" >> return Or)
    <|> (string "!" >> return Not)
    <|> (string "==" >> return Eq)
    <|> (string "!=" >> return Neq)
    <|> (string "<" >> return Lt)
    <|> (string ">" >> return Gt)
    <|> (string "<=" >> return Le)
    <|> (string ">=" >> return Ge)
