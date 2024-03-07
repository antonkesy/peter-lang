module Interpreter.Interpreter (module Interpreter.Interpreter) where

import AST
import Control.Monad (foldM)
import Data.Map.Strict as Map
import Interpreter.BuiltIn
import Interpreter.ProgramState
import Interpreter.Validator

interpret :: Program -> IO ()
interpret (Program statements) = do
  isValid <- validate (Program statements)
  if isValid
    then do
      -- putStrLn "Valid program"
      _ <- foldM interpretStatement (ProgramState empty allFunctions) (addMainFunctionCall ++ statements)
      -- putStrLn $ "End state: " ++ show endState
      return ()
    else do
      putStrLn "Invalid program"
  where
    addMainFunctionCall = if hasMainFunction then mainFunctionCall else []
    mainFunctionCall = [ExpressionStatement (AtomicExpression (FunctionCallAtomic "main" []))]
    hasMainFunction = any isMainFunction statements
    isMainFunction (FunctionDefinitionStatement (Function "main" _ _ _)) = True
    isMainFunction _ = False
    isFunctionDefinition (FunctionDefinitionStatement _) = True
    isFunctionDefinition _ = False
    getFunctionName (FunctionDefinitionStatement (Function name _ _ _)) = name
    allFunctions = Map.fromList $ Prelude.map (\item -> (getFunctionName item, item)) (Prelude.filter isFunctionDefinition statements)

interpretStatement :: ProgramState -> Statement -> IO ProgramState
interpretStatement state (VariableStatement (Variable (VariableDeclaration name _) expression)) = do
  value <- interpretExpression state expression
  return (updateState state name value)
interpretStatement state (AssignmentStatement (Assignment name expression)) = do
  value <- interpretExpression state expression
  return (updateState state name value)
interpretStatement state (ExpressionStatement expression) = do
  _ <- interpretExpression state expression
  return state
interpretStatement state (FunctionDefinitionStatement _) = do
  return state

updateState :: ProgramState -> Name -> Value -> ProgramState
updateState (ProgramState vars funs) name value = ProgramState (Map.insert name value vars) funs

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
interpretAtomic (ProgramState vars _) (VariableAtomic name) = do
  let varValue = Map.lookup name vars
  return $ case varValue of
    Just value -> value
    Nothing -> error $ "Variable not found: " ++ name
interpretAtomic (ProgramState vars funs) (FunctionCallAtomic name args) = do
  let isBuiltIn = Map.lookup name getAllBuiltIns
  case isBuiltIn of
    Just (BuiltIn _ outputType fn) -> do
      argValues <- mapM (\a -> interpretExpression (ProgramState vars funs) a) args
      _ <- fn argValues
      return UnitValue
    Nothing -> do
      let fun = Map.lookup name funs
      case fun of
        Just (FunctionDefinitionStatement (Function _ _ _ body)) -> do
          _ <- foldM interpretStatement (ProgramState vars funs) body
          return UnitValue -- TODO: add return values
        Nothing -> error $ "Function not found: " ++ name

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

interpretOperation :: Operator -> Value -> Value -> Value
interpretOperation Plus (IntValue left) (IntValue right) = IntValue $ left + right
interpretOperation Plus (FloatValue left) (FloatValue right) = FloatValue $ left + right
interpretOperation Minus (IntValue left) (IntValue right) = IntValue $ left - right
interpretOperation Minus (FloatValue left) (FloatValue right) = FloatValue $ left - right
interpretOperation Multiply (IntValue left) (IntValue right) = IntValue $ left * right
interpretOperation Multiply (FloatValue left) (FloatValue right) = FloatValue $ left * right
interpretOperation Divide (IntValue left) (IntValue right) = IntValue $ left `div` right
interpretOperation operator left right = error $ "Unsupported operation: " ++ show operator ++ " " ++ show left ++ " " ++ show right
