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
interpretStatement (InterpretState state _) (ControlStatement control) = do
  interpretControl state control

updateState :: ProgramState -> Name -> Value -> ProgramState
updateState (ProgramState vars funs) name value = ProgramState (Map.insert name value vars) funs

-- Update variable in outer scope
updateOuterState :: ProgramState -> ProgramState -> ProgramState
updateOuterState (ProgramState outerVars funs) (ProgramState innerVars _) =
  ProgramState (Map.unionWithKey (\_ inner _outer -> inner) innerVars outerVars) funs

-- if Map.member name vars then updateState (ProgramState vars funs) name value else ProgramState vars funs

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
      argValues <- getArgValues args
      fn argValues
    Nothing -> do
      let fun = Map.lookup name funs
      case fun of
        Just (FunctionDefinitionStatement (Function _ argDef _ body)) -> do
          params <- mapExpressionToParam argDef args
          let fnScope = ProgramState (Map.union params vars) funs
          (InterpretState _ ret) <- foldM interpretStatement (InterpretState fnScope Nothing) body
          case ret of
            Just value -> return value
            Nothing -> error $ "Function did not return a value: " ++ name
        Nothing -> error $ "Function not found: " ++ name
  where
    getArgValues :: [Expression] -> IO [Value]
    getArgValues = mapM (interpretExpression (ProgramState vars funs))
    mapExpressionToParam :: [VariableDeclaration] -> [Expression] -> IO (Map Name Value)
    mapExpressionToParam [] [] = pure Map.empty
    mapExpressionToParam (VariableDeclaration n _ : rest) (expression : restExp) = do
      val <- interpretExpression (ProgramState vars funs) expression
      restMap <- mapExpressionToParam rest restExp
      return (Map.insert n val restMap)
    mapExpressionToParam _ _ = error "Invalid number of arguments"

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
interpretOperation Lt (IntValue left) (IntValue right) = BoolValue $ left < right
interpretOperation Gt (IntValue left) (IntValue right) = BoolValue $ left > right
interpretOperation Le (IntValue left) (IntValue right) = BoolValue $ left <= right
interpretOperation Ge (IntValue left) (IntValue right) = BoolValue $ left >= right
interpretOperation Eq (IntValue left) (IntValue right) = BoolValue $ left == right
interpretOperation operator left right = error $ "Unsupported operation: " ++ show operator ++ " " ++ show left ++ " " ++ show right

interpretControl :: ProgramState -> Control -> IO InterpretState
interpretControl (ProgramState vars funs) (IfControl test body elseBody) = do
  (BoolValue testValue) <- isTestValue (ProgramState vars funs) test
  if testValue
    then do
      (InterpretState innerVars ret) <- foldM interpretStatement (InterpretState (ProgramState vars funs) Nothing) body
      return $ InterpretState (updateOuterState (ProgramState vars funs) innerVars) ret
    else do
      case elseBody of
        Just elseStatements -> do
          (InterpretState innerVars ret) <- foldM interpretStatement (InterpretState (ProgramState vars funs) Nothing) elseStatements
          return $ InterpretState (updateOuterState (ProgramState vars funs) innerVars) ret
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
  testValue <- interpretExpression (ProgramState vars funs) test
  if not (isBoolValue testValue)
    then do error "Control statement test must be a boolean value."
    else return testValue
  where
    isBoolValue :: Value -> Bool
    isBoolValue (BoolValue _) = True
    isBoolValue _ = False
