module Main (main) where

import Interpreter.Interpreter
import Parser.Program
import Text.Parsec (parse)

developProgram :: String
developProgram =
  -- "int i = 1; int j = 2; int l = 3 + 4; int k = i + j + l;"
  "int i = 1; int j = 2; int l = 3 + 4; int k = i + j + l; k = k * 0;"

main :: IO ()
main = do
  let result = parse parseProgram "" developProgram
  case result of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right program -> do
      putStrLn "Parsed program:"
      print program
      interpret program
