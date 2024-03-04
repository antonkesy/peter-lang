module Parser.Program (module Parser.Program) where

import AST
import Parser.Statement
import Text.Parsec
import Text.Parsec.String

-- TODO: rename to File
parseProgram :: Parser Program
parseProgram = Program <$> (many parseStatement) <* eof
