module Interpreter.Interpreter (module Interpreter.Interpreter) where

import AST
import Control.Monad (foldM, foldM_)
import qualified Data.Functor
import Data.Map.Strict as Map
import Interpreter.BuiltIn
import Interpreter.Literal
import Interpreter.Manipulator
import Interpreter.Operation
import Interpreter.ProgramState
import Interpreter.Validator

interpret :: Program -> IO ()
interpret (Program statements) = do
  isValid <- validate (Program statements)
  if isValid
    then do
      let correctedStatments = ensureEntryPoint statements
          functionMap = getFunctionMap correctedStatments
          customTypeMap = getCustomTypeMap correctedStatments
      foldM_ interpretStatement (InterpretState (ProgramState empty functionMap customTypeMap) Nothing) correctedStatments
    else error "Invalid program"

interpretStatement :: InterpretState -> Statement -> IO InterpretState
interpretStatement (InterpretState state _) (VariableDefinitionStatement (Variable (VariableDeclaration name t) expression)) = do
  (ScopeResult innerVars ret) <- interpretExpression state expression
  let newState = updateOuterStateValue state innerVars
  return (InterpretState (addVariable newState name t ret) Nothing)
interpretStatement (InterpretState state _) (VariableDeclarationStatement (VariableDeclaration name t)) = do
  let newState = addVariable state name t (Just UndefinedValue)
  return (InterpretState newState Nothing)
interpretStatement (InterpretState state _) (AssignmentStatement (Assignment name expression)) = do
  (ScopeResult innerVars ret) <- interpretExpression state expression
  let newState = updateOuterStateValue state innerVars
  return (InterpretState (updateVariable newState name ret) Nothing)
interpretStatement (InterpretState state _) (ExpressionStatement expression) = do
  _ <- interpretExpression state expression
  return (InterpretState state Nothing)
interpretStatement (InterpretState state _) (FunctionDefinitionStatement _) = do
  return (InterpretState state Nothing)
interpretStatement (InterpretState state _) (ReturnStatement expression) = do
  (ScopeResult innerVars ret) <- interpretExpression state expression
  let newState = updateOuterStateValue state innerVars
  return (InterpretState newState ret)
interpretStatement (InterpretState state _) (ControlStatement control) = do
  interpretControl state control
interpretStatement (InterpretState state _) (StructStatement _) = do
  return (InterpretState state Nothing)

interpretExpression :: ProgramState -> Expression -> IO ScopeResult
interpretExpression state (AtomicExpression atomic) = do
  interpretAtomic state atomic
interpretExpression state (OperationExpression left operator right) = do
  (ScopeResult _ (Just leftValue)) <- interpretExpression state left
  (ScopeResult _ (Just rightValue)) <- interpretExpression state right
  let value = interpretOperation operator leftValue rightValue
  return (ScopeResult (variables state) (Just value))

interpretAtomic :: ProgramState -> Atomic -> IO ScopeResult
interpretAtomic (ProgramState vars _ _) (LiteralAtomic literal) = do
  ret <- interpretLiteral literal
  return (ScopeResult vars (Just ret))
interpretAtomic (ProgramState vars f t) (VariableAtomic name) =
  let varValue = getVariable (ProgramState vars f t) name
   in return (ScopeResult vars varValue)
interpretAtomic (ProgramState vars funs t) (FunctionCallAtomic name args) = do
  let isBuiltIn = Map.lookup name getAllBuiltIns
  case isBuiltIn of
    Just (BuiltIn _ _ fn) -> do
      argValues <- getArgValues args
      ret <- fn argValues
      return (ScopeResult vars (Just ret))
    Nothing -> do
      let fun = Map.lookup name funs
      case fun of
        Just (FunctionDefinitionStatement (Function _ argDef _ body)) -> do
          params <- mapExpressionToParam argDef args
          let fnScope = ProgramState (Map.union params vars) funs t
          (ScopeResult innerVars ret) <- returnSkipWrapper (InterpretState fnScope Nothing) body True
          let (ProgramState newVars _ _) = updateOuterStateValue (ProgramState vars funs t) innerVars
          return (ScopeResult newVars ret)
        _ -> error $ "Function not found: " ++ name
  where
    getArgValues :: [Expression] -> IO [Value]
    getArgValues exprs =
      mapM
        (interpretExpression (ProgramState vars funs t))
        exprs
        Data.Functor.<&> Prelude.map getValue
    getValue :: ScopeResult -> Value
    getValue (ScopeResult _ (Just v)) = v
    getValue (ScopeResult _ Nothing) = error "Value not found"

    mapExpressionToParam :: [VariableDeclaration] -> [Expression] -> IO (Map Name Value)
    mapExpressionToParam [] [] = pure Map.empty
    mapExpressionToParam (VariableDeclaration n _ : rest) (expression : restExp) = do
      (ScopeResult _ (Just val)) <- interpretExpression (ProgramState vars funs t) expression
      restMap <- mapExpressionToParam rest restExp
      return (Map.insert n val restMap)
    mapExpressionToParam _ _ = error "Invalid number of arguments"

returnSkipWrapper :: InterpretState -> [Statement] -> Bool -> IO ScopeResult
returnSkipWrapper state (stmt : rest) inFunction = do
  (InterpretState s ret) <- interpretStatement state stmt
  case ret of
    Just value -> return (ScopeResult (variables s) (Just value))
    Nothing -> returnSkipWrapper (InterpretState s Nothing) rest inFunction
returnSkipWrapper state [] inFunction =
  if inFunction
    then error "missing return"
    else return (ScopeResult (variables (programState state)) Nothing)

interpretControl :: ProgramState -> Control -> IO InterpretState
interpretControl (ProgramState vars funs t) (IfControl test body elseBody) = do
  (BoolValue testValue) <- isTestValue (ProgramState vars funs t) test
  if testValue
    then do
      (ScopeResult innerVars ret) <- returnSkipWrapper (InterpretState (ProgramState vars funs t) Nothing) body False
      return $ InterpretState (updateOuterStateValue (ProgramState vars funs t) innerVars) ret
    else do
      case elseBody of
        Just elseStatements -> do
          -- TODO: extract cancellable statements function
          (ScopeResult innerVars ret) <- returnSkipWrapper (InterpretState (ProgramState vars funs t) Nothing) elseStatements False
          return $ InterpretState (updateOuterStateValue (ProgramState vars funs t) innerVars) ret
        Nothing -> return $ InterpretState (ProgramState vars funs t) Nothing
interpretControl (ProgramState vars funs t) (WhileControl test body) = do
  (BoolValue testValue) <- isTestValue (ProgramState vars funs t) test
  if testValue
    then do
      (InterpretState innerVars ret) <- foldM interpretStatement (InterpretState (ProgramState vars funs t) Nothing) body
      case ret of
        Just value -> return $ InterpretState (updateOuterState (ProgramState vars funs t) innerVars) (Just value)
        Nothing -> interpretControl (updateOuterState (ProgramState vars funs t) innerVars) (WhileControl test body)
    else return $ InterpretState (ProgramState vars funs t) Nothing

isTestValue :: ProgramState -> Expression -> IO Value
isTestValue s test = do
  (ScopeResult _ (Just testValue)) <- interpretExpression s test
  if not (isBoolValue testValue)
    then do error "Control statement test must be a boolean value."
    else return testValue
  where
    isBoolValue :: Value -> Bool
    isBoolValue (BoolValue _) = True
    isBoolValue _ = False
