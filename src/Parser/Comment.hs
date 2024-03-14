module Parser.Comment (module Parser.Comment) where

import AST
import Control.Monad (void)
import Parser.EndOfLine (eol)
import Text.Parsec
import Text.Parsec.String

consumeComment :: Parser ()
consumeComment =
  void
    ( try parseSingleLineComment
        <|> parseMultiLineComment
    )

parseComment :: Parser Comment
parseComment =
  try parseSingleLineComment <|> parseMultiLineComment

parseSingleLineComment :: Parser Comment
parseSingleLineComment =
  string "//" *> manyTill anyChar eol

parseMultiLineComment :: Parser Comment
parseMultiLineComment =
  string "/*" *> manyTill anyChar (string "*/")
