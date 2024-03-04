module Parser.Comment
  ( parseComment,
    parseSingleLineComment,
    parseMultiLineComment,
    consumeComment,
  )
where

import AST
import Control.Monad (void)
import Parser.EndOfLine (eol)
import Text.Parsec
import Text.Parsec.String

consumeComment :: Parser ()
consumeComment =
  void
    ( try parseSingleLineComment
        <|> try parseMultiLineComment
    )

parseComment :: Parser Comment
parseComment =
  try parseSingleLineComment <|> try parseMultiLineComment

parseSingleLineComment :: Parser Comment
parseSingleLineComment =
  string "//" *> manyTill anyChar (try eol)

parseMultiLineComment :: Parser Comment
parseMultiLineComment =
  string "/*" *> manyTill anyChar (try (string "*/"))
