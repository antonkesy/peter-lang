module Interpreter.Literal (module Interpreter.Literal) where

import AST
import Interpreter.ProgramState

interpretLiteral :: Literal -> IO Value
interpretLiteral (IntLiteral value) = do
  return $ IntValue value
interpretLiteral (FloatLiteral value) = do
  return $ FloatValue value
interpretLiteral (BoolLiteral value) = do
  return $ BoolValue value
interpretLiteral UnitLiteral = do
  return UnitValue
interpretLiteral (StringLiteral value) = do
  return $ StringValue value
interpretLiteral UndefinedLiteral = do
  return UndefinedValue
