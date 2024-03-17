{-# LANGUAGE GADTs #-}

module Interpreter.ProgramState (module Interpreter.ProgramState) where

import AST
import Data.List (intercalate)
import Data.List.Split
import Data.Map.Strict as Map

data Value
  = IntValue Int
  | FloatValue Float
  | BoolValue Bool
  | UnitValue
  | StringValue String
  | InterpreterErrorValue String
  | UndefinedValue
  | StructValue {structType :: Name, structMembers :: Map Name Value}
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

getVariable :: ProgramState -> Name -> Maybe Value
getVariable (ProgramState vars _ _) varName =
  if isMemberCall varName
    then getEmbeddedStructMember vars varName
    else
      let varValue = Map.lookup varName vars
       in case varValue of
            Nothing -> error ("Variable not found: " ++ varName)
            _ -> varValue
  where
    isMemberCall :: Name -> Bool
    isMemberCall name = length (splitOn "." name) > 1

getEmbeddedStructMember :: Map Name Value -> Name -> Maybe Value
getEmbeddedStructMember vars varName = do
  let baseName = head (splitOn "." varName)
  let nextName = splitOn "." varName !! 1
  let structValue = Map.lookup baseName vars
  case structValue of
    Just (StructValue _ memb) -> do
      let memberValue = Map.lookup nextName memb
      case memberValue of
        Just (StructValue _ memb') -> getEmbeddedStructMember memb' (removeBaseName varName)
        Just v -> Just v
        Nothing -> error ("Member not found: " ++ baseName ++ "." ++ nextName)
    _ ->
      error
        ( "Struct not found: "
            ++ (" name: " ++ baseName ++ " ")
            ++ (" value: " ++ show structValue ++ " ")
            ++ (" vars: " ++ show vars)
            ++ " members: "
            ++ nextName
        )
  where
    removeBaseName :: Name -> Name
    removeBaseName name = intercalate "." (tail (splitOn "." name))

addVariable :: ProgramState -> Name -> Type -> Maybe Value -> ProgramState
addVariable (ProgramState vars funs t) varName (CustomType structName) _ = do
  let s = Just (getStructValue (ProgramState vars funs t) (CustomType structName))
  updateVariable (ProgramState vars funs t) varName s
addVariable (ProgramState vars funs t) varName _ value =
  updateVariable (ProgramState vars funs t) varName value

getStructValue :: ProgramState -> Type -> Value
getStructValue (ProgramState vars funs t) (CustomType structTypeName) = do
  let state = ProgramState vars funs t
  case getStruct state structTypeName of
    Just (Struct _ mems) -> StructValue structTypeName (getMembers state mems)
    Nothing -> error "Struct definition not found"
  where
    getStruct :: ProgramState -> Name -> Maybe Struct
    getStruct (ProgramState _ _ ty) name = Map.lookup name ty
    getMembers :: ProgramState -> [VariableDeclaration] -> Map Name Value
    getMembers _ [] = Map.empty
    getMembers state (VariableDeclaration name ty : rest) = case ty of
      CustomType _ -> getMembers state rest `union` Map.singleton name (getStructValue state ty)
      _ -> do getMembers state rest `union` Map.singleton name UndefinedValue -- TODO: set default value -> remove UndefinedValue
getStructValue _ t = error ("Invalid type: " ++ show t)

updateVariable :: ProgramState -> Name -> Maybe Value -> ProgramState
updateVariable s _ Nothing = s
updateVariable (ProgramState vars funs t) varName (Just value) =
  if not (updatesMember varName)
    then do
      ProgramState (Map.insert varName value vars) funs t
    else do
      let baseName = head (splitOn "." varName)
      let baseStruct = Map.lookup baseName vars
      case baseStruct of
        Just (StructValue n memb) -> do
          let updatedStruct = updateStruct (StructValue n memb) varName value
          updateVariable (ProgramState vars funs t) baseName (Just updatedStruct)
        _ -> error ("Variable not found: " ++ varName)
  where
    updatesMember :: Name -> Bool
    updatesMember name = length (splitOn "." name) > 1

updateStruct :: Value -> Name -> Value -> Value
updateStruct (StructValue n memb) pathName value = do
  let memberName = splitOn "." pathName !! 1
      memberVar = Map.lookup memberName memb
   in case memberVar of
        Just _ -> do
          let newMemb = Map.insert memberName value memb
           in StructValue n newMemb
        Nothing -> error "Member not found"
updateStruct _ _ v = error ("Invalid struct: " ++ show v)

updateOuterState :: ProgramState -> ProgramState -> ProgramState
updateOuterState (ProgramState outerVars funs t) (ProgramState innerVars _ _) =
  ProgramState (Map.unionWithKey (\_ inner _outer -> inner) innerVars outerVars) funs t

updateOuterStateValue :: ProgramState -> Map Name Value -> ProgramState
updateOuterStateValue outerState innerVars =
  let mockState = ProgramState innerVars Map.empty Map.empty
   in updateOuterState outerState mockState
