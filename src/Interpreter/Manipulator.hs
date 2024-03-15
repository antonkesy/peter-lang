module Interpreter.Manipulator (module Interpreter.Manipulator) where

import AST
import Data.Map.Strict as Map

ensureEntryPoint :: [Statement] -> [Statement]
ensureEntryPoint inStatement = addMainFunctionCall ++ inStatement
  where
    addMainFunctionCall = if hasMainFunction then mainFunctionCall else []
    mainFunctionCall = [ExpressionStatement (AtomicExpression (FunctionCallAtomic "main" []))]
    hasMainFunction = any isMainFunction inStatement
    isMainFunction (FunctionDefinitionStatement (Function "main" _ _ _)) = True
    isMainFunction _ = False

getFunctionMap :: [Statement] -> Map Name Statement
getFunctionMap inStatments =
  let rawMap = Map.fromList $ Prelude.map (\item -> (getFunctionName item, item)) (Prelude.filter isFunctionDefinition inStatments)
   in ensureVoidFunctionReturn rawMap
  where
    isFunctionDefinition (FunctionDefinitionStatement _) = True
    isFunctionDefinition _ = False
    getFunctionName (FunctionDefinitionStatement (Function name _ _ _)) = name

getCustomTypeMap :: [Statement] -> Map Name Struct
getCustomTypeMap inStatments =
  let rawMap = Map.fromList $ Prelude.map (\item -> (getStructName item, getStruct item)) (Prelude.filter isStructDefinition inStatments)
   in rawMap
  where
    isStructDefinition (StructStatement _) = True
    isStructDefinition _ = False
    getStructName (StructStatement (Struct name _)) = name
    getStruct (StructStatement s) = s

ensureVoidFunctionReturn :: Map Name Statement -> Map Name Statement
ensureVoidFunctionReturn = Map.mapWithKey ensureVoidReturn
  where
    ensureVoidReturn name (FunctionDefinitionStatement (Function _ args returnType statements)) =
      let updatedStatements =
            if returnType == UnitType
              then statements ++ [ReturnStatement (AtomicExpression (LiteralAtomic UnitLiteral))]
              else statements
       in FunctionDefinitionStatement (Function name args returnType updatedStatements)
