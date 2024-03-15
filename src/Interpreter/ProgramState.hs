{-# LANGUAGE GADTs #-}

module Interpreter.ProgramState (module Interpreter.ProgramState) where

import AST
import Data.Map.Strict as Map

data Value = IntValue Int | FloatValue Float | BoolValue Bool | UnitValue | StringValue String | InterpreterErrorValue String | UndefinedValue
  deriving (Show, Eq)

data ProgramState where
  ProgramState ::
    { variables :: Map Name Value,
      functions :: Map Name Statement,
      types :: Map Name Struct
    } ->
    ProgramState
  deriving (Show, Eq)

data InterpretState where
  InterpretState :: {programState :: ProgramState, returnValue :: Maybe Value} -> InterpretState
  deriving (Show, Eq)

data ScopeResult = ScopeResult (Map Name Value) (Maybe Value)
  deriving (Show, Eq)

updateState :: ProgramState -> Name -> Maybe Value -> ProgramState
updateState (ProgramState vars funs t) name value = do
  case value of
    Just v -> ProgramState (Map.insert name v vars) funs t
    Nothing -> ProgramState vars funs t

-- Update variable in outer scope
updateOuterState :: ProgramState -> ProgramState -> ProgramState
updateOuterState (ProgramState outerVars funs t) (ProgramState innerVars _ _) =
  ProgramState (Map.unionWithKey (\_ inner _outer -> inner) innerVars outerVars) funs t

updateOuterStateV :: ProgramState -> Map Name Value -> ProgramState
updateOuterStateV (ProgramState outerVars funs t) innerVars =
  ProgramState
    (Map.unionWithKey (\_ inner _outer -> inner) innerVars outerVars)
    funs
    t

addStructMembersToState :: ProgramState -> Name -> Type -> ProgramState
addStructMembersToState s varName typeName = do
  case typeName of
    CustomType structName -> do
      let struct = getStruct s structName
      case struct of
        Just struc -> addStructMembersToState' s varName struc
        Nothing -> s
    _ -> s
  where
    getStruct :: ProgramState -> Name -> Maybe Struct
    getStruct (ProgramState _ _ t) name = Map.lookup name t
    addStructMembersToState' :: ProgramState -> Name -> Struct -> ProgramState
    addStructMembersToState' (ProgramState vars funs t) baseName (Struct name members) =
      ProgramState vars funs (Map.insert (baseName ++ "." ++ name) (Struct name members) t)
