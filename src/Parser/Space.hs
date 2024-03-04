module Parser.Space
  ( space',
    spaces',
    spaces1,
  )
where

import Control.Monad (void)
import Parser.Comment (consumeComment)
import Text.Parsec
import Text.Parsec.String

space' :: Parser ()
space' =
  void space
    <|> consumeComment
    <|> void newline
    <|> void tab

spaces' :: Parser ()
spaces' = skipMany space'

spaces1 :: Parser ()
spaces1 = skipMany1 space'
