module Main (main) where

import Parser.Program
import Text.Parsec (parse)

developProgram :: String
developProgram =
  "int i = 1; int j = 2; int k = i + j;"

main :: IO ()
main = do
  let result = parse parseProgram "" developProgram
  case result of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right program -> do
      putStrLn "Parsed program:"
      print program
