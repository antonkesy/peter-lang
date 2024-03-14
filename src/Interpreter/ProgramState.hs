{-# LANGUAGE GADTs #-}

module Interpreter.ProgramState (module Interpreter.ProgramState) where

import AST
import Data.Map.Strict as Map

data Value = IntValue Int | FloatValue Float | BoolValue Bool | UnitValue | StringValue String | InterpreterErrorValue String
  deriving (Show, Eq)

data ProgramState where
  ProgramState :: {variables :: Map Name Value, functions :: Map Name Statement} -> ProgramState
  deriving (Show, Eq)

data InterpretState where
  InterpretState :: {programState :: ProgramState, returnValue :: Maybe Value} -> InterpretState
  deriving (Show, Eq)

data ScopeResult = ScopeResult (Map Name Value) (Maybe Value)
  deriving (Show, Eq)
