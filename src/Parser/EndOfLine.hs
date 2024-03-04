module Parser.EndOfLine (eol) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

eol :: Parser ()
eol = void (char '\n') <|> eof
