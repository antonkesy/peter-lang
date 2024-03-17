module Parser.Comment (module Parser.Comment) where

import Control.Monad (void)
import Parser.EndOfLine (eol)
import Text.Parsec
import Text.Parsec.String

type Comment = String

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
  string "//" *> manyTill anyChar eoi
  where
    eoi = void eol <|> lookAhead eof -- don't consume eof

parseMultiLineComment :: Parser Comment
parseMultiLineComment =
  string "/*" *> manyTill anyChar (try (string "*/"))
