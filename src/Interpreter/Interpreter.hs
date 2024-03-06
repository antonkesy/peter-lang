{-# LANGUAGE GADTs #-}

module Interpreter.Interpreter (module Interpreter.Interpreter) where

import AST
import Control.Monad (foldM)
import Data.Map.Strict as Map

data Value = IntValue Int | FloatValue Float | BoolValue Bool | UnitValue
  deriving (Show)

data ProgramState where
  ProgramState :: {variables :: Map Name Value} -> ProgramState
  deriving (Show)

interpret :: Program -> IO ()
interpret (Program statements) = do
  endState <- foldM interpretStatement (ProgramState empty) statements
  putStrLn $ "End state: " ++ show endState

interpretStatement :: ProgramState -> Statement -> IO ProgramState
interpretStatement state (VariableStatement (Variable name _ expression)) = do
  value <- interpretExpression state expression
  return (updateState state name value)
interpretStatement state (AssignmentStatement (Assignment name expression)) = do
  value <- interpretExpression state expression
  return (updateState state name value)

updateState :: ProgramState -> Name -> Value -> ProgramState
updateState (ProgramState vars) name value = ProgramState $ Map.insert name value vars

interpretExpression :: ProgramState -> Expression -> IO Value
interpretExpression state (AtomicExpression atomic) = do
  interpretAtomic state atomic
interpretExpression state (OperationExpression left operator right) = do
  leftValue <- interpretExpression state left
  rightValue <- interpretExpression state right
  let value = interpretOperation operator leftValue rightValue
  return value

interpretAtomic :: ProgramState -> Atomic -> IO Value
interpretAtomic _ (LiteralAtomic literal) = do
  interpretLiteral literal
interpretAtomic (ProgramState vars) (VariableAtomic name) = do
  let varValue = Map.lookup name vars
  return $ case varValue of
    Just value -> value
    Nothing -> error $ "Variable not found: " ++ name

interpretLiteral :: Literal -> IO Value
interpretLiteral (IntLiteral value) = do
  return $ IntValue value
interpretLiteral (FloatLiteral value) = do
  return $ FloatValue value
interpretLiteral (BoolLiteral value) = do
  return $ BoolValue value
interpretLiteral UnitLiteral = do
  return UnitValue

interpretOperation :: Operator -> Value -> Value -> Value
interpretOperation Plus (IntValue left) (IntValue right) = IntValue $ left + right
interpretOperation Plus (FloatValue left) (FloatValue right) = FloatValue $ left + right
interpretOperation Minus (IntValue left) (IntValue right) = IntValue $ left - right
interpretOperation Minus (FloatValue left) (FloatValue right) = FloatValue $ left - right
interpretOperation Multiply (IntValue left) (IntValue right) = IntValue $ left * right
interpretOperation Multiply (FloatValue left) (FloatValue right) = FloatValue $ left * right
interpretOperation Divide (IntValue left) (IntValue right) = IntValue $ left `div` right
interpretOperation operator left right = error $ "Unsupported operation: " ++ show operator ++ " " ++ show left ++ " " ++ show right
