module Interpreter.BuiltIn (module Interpreter.BuiltIn) where

import AST
import Data.Map.Strict as Map

-- import Text.Parsec
-- import Text.Parsec.String

-- printBuiltIn :: Parser Statement

data BuiltIn = BuiltIn Name [Type] Type ([Type] -> IO Type)

getAllBuiltIns :: Map String BuiltIn
getAllBuiltIns = Map.fromList [("print", printBuiltIn)]

printBuiltIn :: BuiltIn
printBuiltIn =
  BuiltIn
    "print"
    [CustomType "String"]
    UnitType
    ( \[CustomType "String"] -> do
        putStrLn "Hello, World!"
        pure UnitType
    )
