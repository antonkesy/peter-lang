module Interpreter.Validator (module Interpreter.Validator) where

import AST

validate :: Program -> IO Bool
validate program = do
  allChecks <- sequence [hasStatements program, hasEntryPoint program]
  return (and allChecks)

hasStatements :: Program -> IO Bool
hasStatements (Program statements) =
  if not (null statements)
    then return True
    else do
      putStrLn "Program has no statements"
      return False

-- ensures that the program has only a SINGLE entry point
-- either a main function or a global statements which assign to a variable without a function or declaration
hasEntryPoint :: Program -> IO Bool
hasEntryPoint (Program statements) =
  let countMainFunctions = length $ filter isMainFunction statements
      countStatements = length $ filter isGlobalStatement statements
   in if countMainFunctions == 1 && countStatements == 0 || countMainFunctions == 0 && countStatements > 0
        then return True
        else do
          putStrLn "Program has no entry point or has multiple entry points. A program must have a single entry point."
          return False
  where
    isMainFunction (FunctionDefinitionStatement (Function "main" _ _ _)) = True
    isMainFunction _ = False
    isGlobalStatement (AssignmentStatement _) = True
    isGlobalStatement (ExpressionStatement _) = True
    isGlobalStatement _ = False

-- TODO: check no name clash with built-in functions
