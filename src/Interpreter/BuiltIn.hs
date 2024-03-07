module Interpreter.BuiltIn (module Interpreter.BuiltIn) where

import AST
import Data.Map.Strict as Map
import Interpreter.ProgramState

data BuiltIn = BuiltIn Name Type ([Value] -> IO Value)

getAllBuiltIns :: Map String BuiltIn
getAllBuiltIns = Map.fromList [("print", printBuiltIn)]

printBuiltIn :: BuiltIn
printBuiltIn =
  BuiltIn
    "print"
    UnitType
    ( \val -> do
        case val of
          [(StringValue s)] -> putStrLn s
          _ -> error "Not a single string"
        pure UnitValue
    )
