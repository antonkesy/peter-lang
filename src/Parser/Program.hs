module Parser.Program (module Parser.Program) where

import AST
import Parser.Statement
import Text.Parsec
import Text.Parsec.String

-- TODO: rename to File
parseProgram :: Parser Program
parseProgram = do
  statements <- many (try parseStatement)
  -- _ <- eof -- TODO: ensure no tokens left -> EOF not working becuase already consumed
  _ <- optional eof
  return $ Program statements
