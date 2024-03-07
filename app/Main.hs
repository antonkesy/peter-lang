module Main (main) where

import Interpreter.Interpreter
import Options.Applicative
import Parser.Program
import Text.Parsec (parse)

-- TODO: replace with either
data Options = Options (Maybe String) (Maybe FilePath)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> optional
      ( strOption
          ( long "inline"
              <> short 'i'
              <> metavar "STRING"
              <> help "Inline string to parse and interpret"
          )
      )
    <*> optional (argument str (metavar "PATH"))

main :: IO ()
main = do
  opts <- execParser $ info (parseOptions <**> helper) fullDesc
  processOptions opts

processOptions :: Options -> IO ()
processOptions (Options Nothing (Just path)) = do
  contents <- readFile path
  runPeter contents
processOptions (Options (Just inlineSourceCode) Nothing) =
  runPeter inlineSourceCode
processOptions _ =
  putStrLn "Arguments: provided file path OR inline source code"

runPeter :: String -> IO ()
runPeter sourceCode = do
  let result = parse parseProgram "" sourceCode
  case result of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right program -> do
      -- putStrLn "Parsed program:"
      print program
      interpret program
