import E2E.Placeholder
import Test.HUnit (Test (TestList), runTestTTAndExit)
import Unit.Parser.Comment

main :: IO ()
main =
  runTestTTAndExit
    ( TestList
        ( Unit.Parser.Comment.allTests
            ++ E2E.Placeholder.allTests
        )
    )
