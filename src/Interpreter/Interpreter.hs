module Interpreter.Interpreter (module Interpreter.Interpreter) where

import AST
import Control.Monad (foldM)
import Data.Map.Strict as Map
import Interpreter.BuiltIn
import Interpreter.Manipulator
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
      _ <- foldM interpretStatement (InterpretState (ProgramState empty functionMap) Nothing) correctedStatments
      -- putStrLn $ "End state: " ++ show endState
      return ()
    else do
      putStrLn "Invalid program"

interpretStatement :: InterpretState -> Statement -> IO InterpretState
interpretStatement (InterpretState state _) (VariableStatement (Variable (VariableDeclaration name _) expression)) = do
  value <- interpretExpression state expression
  return (InterpretState (updateState state name value) Nothing)
interpretStatement (InterpretState state _) (AssignmentStatement (Assignment name expression)) = do
  value <- interpretExpression state expression
  return (InterpretState (updateState state name value) Nothing)
interpretStatement (InterpretState state _) (ExpressionStatement expression) = do
  _ <- interpretExpression state expression
  return (InterpretState state Nothing)
interpretStatement (InterpretState state _) (FunctionDefinitionStatement _) = do
  return (InterpretState state Nothing)
interpretStatement (InterpretState state _) (ReturnStatement expression) = do
  ret <- interpretExpression state expression
  return (InterpretState state (Just ret))

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
    Just (BuiltIn _ _ fn) -> do
      argValues <- mapM (interpretExpression (ProgramState vars funs)) args
      fn argValues
    Nothing -> do
      let fun = Map.lookup name funs
      case fun of
        Just (FunctionDefinitionStatement (Function _ _ _ body)) -> do
          (InterpretState _ ret) <- foldM interpretStatement (InterpretState (ProgramState vars funs) Nothing) body
          case ret of
            Just value -> return value
            Nothing -> error $ "Function did not return a value: " ++ name
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
