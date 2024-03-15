module Interpreter.BuiltIn (getAllBuiltIns, BuiltIn (..)) where

import AST
import Data.Map.Strict as Map
import qualified Data.Text as T
import Interpreter.ProgramState
import System.IO

data BuiltIn = BuiltIn Name Type ([Value] -> IO Value)

getAllBuiltIns :: Map String BuiltIn
getAllBuiltIns =
  Map.fromList
    [ ("print", Interpreter.BuiltIn.print),
      ("println", printLn),
      ("str", toString),
      ("int", toInt),
      ("float", toFloat),
      ("input", getInput)
    ]

print :: BuiltIn
print =
  BuiltIn
    "print"
    UnitType
    ( \val -> do
        case val of
          [StringValue s] -> do
            putStr (replaceEscaped s)
            hFlush stdout
          _ -> error "Not a single string"
        pure UnitValue
    )
  where
    replaceEscaped :: String -> String
    replaceEscaped s =
      T.unpack (replaceEscaped' ("\\n", "\n") (T.pack s))
      where
        replaceEscaped' (x, y) = T.replace (T.pack x) (T.pack y)

printLn :: BuiltIn
printLn =
  BuiltIn
    "println"
    UnitType
    ( \val -> do
        case val of
          [StringValue s] -> putStrLn (replaceEscaped s)
          _ -> error "Not a single string"
        pure UnitValue
    )
  where
    replaceEscaped :: String -> String
    replaceEscaped s =
      T.unpack (replaceEscaped' ("\\n", "\n") (T.pack s))
      where
        replaceEscaped' (x, y) = T.replace (T.pack x) (T.pack y)

toString :: BuiltIn
toString =
  BuiltIn
    "str"
    StringType
    ( \val -> do
        pure (StringValue (valueToString val))
    )
  where
    valueToString :: [Value] -> String
    valueToString val = case val of
      [StringValue s] -> s
      [IntValue i] -> show i
      [FloatValue f] -> show f
      [BoolValue b] -> show b
      _ -> error ("No matching type for str: " ++ show val)

getInput :: BuiltIn
getInput =
  BuiltIn
    "input"
    StringType
    ( \_ -> do
        StringValue <$> getLine
    )

toInt :: BuiltIn
toInt =
  BuiltIn
    "toInt"
    IntType
    ( \val -> do
        pure (IntValue (valueToInt val))
    )
  where
    valueToInt :: [Value] -> Int
    valueToInt val = case val of
      [IntValue i] -> i
      [StringValue s] -> read s
      _ -> error ("No matching type for toInt: " ++ show val)

toFloat :: BuiltIn
toFloat =
  BuiltIn
    "toFloat"
    FloatType
    ( \val -> do
        pure (FloatValue (valueToFloat val))
    )
  where
    valueToFloat :: [Value] -> Float
    valueToFloat val = case val of
      [FloatValue f] -> f
      [StringValue s] -> read s
      _ -> error ("No matching type for toFloat: " ++ show val)
