{-# LANGUAGE GADTs #-}

module Interpreter.ProgramState (module Interpreter.ProgramState) where

import AST
import Data.Map.Strict as Map

data Value = IntValue Int | FloatValue Float | BoolValue Bool | UnitValue | StringValue String
  deriving (Show)

data ProgramState where
  ProgramState :: {variables :: Map Name Value, functions :: Map Name Statement} -> ProgramState
  deriving (Show)
