module Parser.EndOfLine (module Parser.EndOfLine) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

eol :: Parser ()
eol = void endOfLine <|> eof
