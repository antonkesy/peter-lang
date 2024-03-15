module Interpreter.Interpreter (module Interpreter.Interpreter) where

import AST
import Control.Monad (foldM)
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
      -- putStrLn "Valid program"
      let correctedStatments = ensureEntryPoint statements
      let functionMap = getFunctionMap correctedStatments
      -- print correctedStatments
      _ <- foldM interpretStatement (InterpretState (ProgramState empty functionMap) Nothing) correctedStatments
      -- putStrLn $ "End state: " ++ show endState
      return ()
    else do
      putStrLn "Invalid program"

interpretStatement :: InterpretState -> Statement -> IO InterpretState
interpretStatement (InterpretState state _) (VariableStatement (Variable (VariableDeclaration name _) expression)) = do
  (ScopeResult innerVars ret) <- interpretExpression state expression
  let newState = updateOuterStateV state innerVars
  return (InterpretState (updateState newState name ret) Nothing)
interpretStatement (InterpretState state _) (AssignmentStatement (Assignment name expression)) = do
  (ScopeResult innerVars ret) <- interpretExpression state expression
  let newState = updateOuterStateV state innerVars
  return (InterpretState (updateState newState name ret) Nothing)
interpretStatement (InterpretState state _) (ExpressionStatement expression) = do
  _ <- interpretExpression state expression
  return (InterpretState state Nothing)
interpretStatement (InterpretState state _) (FunctionDefinitionStatement _) = do
  return (InterpretState state Nothing)
interpretStatement (InterpretState state _) (ReturnStatement expression) = do
  (ScopeResult innerVars ret) <- interpretExpression state expression
  let newState = updateOuterStateV state innerVars
  return (InterpretState newState ret)
interpretStatement (InterpretState state _) (ControlStatement control) = do
  interpretControl state control

updateState :: ProgramState -> Name -> Maybe Value -> ProgramState
updateState (ProgramState vars funs) name value = do
  case value of
    Just v -> ProgramState (Map.insert name v vars) funs
    Nothing -> ProgramState vars funs

-- Update variable in outer scope
updateOuterState :: ProgramState -> ProgramState -> ProgramState
updateOuterState (ProgramState outerVars funs) (ProgramState innerVars _) =
  ProgramState (Map.unionWithKey (\_ inner _outer -> inner) innerVars outerVars) funs

updateOuterStateV :: ProgramState -> Map Name Value -> ProgramState
updateOuterStateV (ProgramState outerVars funs) innerVars =
  ProgramState
    (Map.unionWithKey (\_ inner _outer -> inner) innerVars outerVars)
    funs

interpretExpression :: ProgramState -> Expression -> IO ScopeResult
interpretExpression state (AtomicExpression atomic) = do
  interpretAtomic state atomic
interpretExpression state (OperationExpression left operator right) = do
  (ScopeResult _ (Just leftValue)) <- interpretExpression state left
  (ScopeResult _ (Just rightValue)) <- interpretExpression state right
  let value = interpretOperation operator leftValue rightValue
  return (ScopeResult (variables state) (Just value))

interpretAtomic :: ProgramState -> Atomic -> IO ScopeResult
interpretAtomic (ProgramState vars _) (LiteralAtomic literal) = do
  ret <- interpretLiteral literal
  return (ScopeResult vars (Just ret))
interpretAtomic (ProgramState vars _) (VariableAtomic name) = do
  let varValue = Map.lookup name vars
  return $ case varValue of
    Just value -> ScopeResult vars (Just value)
    Nothing -> error $ "Variable not found: " ++ name
interpretAtomic (ProgramState vars funs) (FunctionCallAtomic name args) = do
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
          let fnScope = ProgramState (Map.union params vars) funs
          (ScopeResult innerVars ret) <- returnSkipWrapper (InterpretState fnScope Nothing) body True
          let (ProgramState newVars _) = updateOuterStateV (ProgramState vars funs) innerVars
          return (ScopeResult newVars ret)
        _ -> error $ "Function not found: " ++ name
  where
    getArgValues :: [Expression] -> IO [Value]
    getArgValues exprs =
      mapM
        (interpretExpression (ProgramState vars funs))
        exprs
        Data.Functor.<&> Prelude.map (\(ScopeResult _ (Just v)) -> v)

    mapExpressionToParam :: [VariableDeclaration] -> [Expression] -> IO (Map Name Value)
    mapExpressionToParam [] [] = pure Map.empty
    mapExpressionToParam (VariableDeclaration n _ : rest) (expression : restExp) = do
      (ScopeResult _ (Just val)) <- interpretExpression (ProgramState vars funs) expression
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
interpretControl (ProgramState vars funs) (IfControl test body elseBody) = do
  (BoolValue testValue) <- isTestValue (ProgramState vars funs) test
  if testValue
    then do
      (ScopeResult innerVars ret) <- returnSkipWrapper (InterpretState (ProgramState vars funs) Nothing) body False
      return $ InterpretState (updateOuterStateV (ProgramState vars funs) innerVars) ret
    else do
      case elseBody of
        Just elseStatements -> do
          -- TODO: extract cancellable statements function
          (ScopeResult innerVars ret) <- returnSkipWrapper (InterpretState (ProgramState vars funs) Nothing) elseStatements False
          return $ InterpretState (updateOuterStateV (ProgramState vars funs) innerVars) ret
        Nothing -> return $ InterpretState (ProgramState vars funs) Nothing
interpretControl (ProgramState vars funs) (WhileControl test body) = do
  (BoolValue testValue) <- isTestValue (ProgramState vars funs) test
  if testValue
    then do
      (InterpretState innerVars ret) <- foldM interpretStatement (InterpretState (ProgramState vars funs) Nothing) body
      case ret of
        Just value -> return $ InterpretState (updateOuterState (ProgramState vars funs) innerVars) (Just value)
        Nothing -> interpretControl (updateOuterState (ProgramState vars funs) innerVars) (WhileControl test body)
    else return $ InterpretState (ProgramState vars funs) Nothing

isTestValue :: ProgramState -> Expression -> IO Value
isTestValue (ProgramState vars funs) test = do
  (ScopeResult _ (Just testValue)) <- interpretExpression (ProgramState vars funs) test
  if not (isBoolValue testValue)
    then do error "Control statement test must be a boolean value."
    else return testValue
  where
    isBoolValue :: Value -> Bool
    isBoolValue (BoolValue _) = True
    isBoolValue _ = False
