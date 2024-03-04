import E2E.Placeholder
import Test.HUnit (Test (TestList), runTestTTAndExit)
import Unit.Placeholder

main :: IO ()
main =
  runTestTTAndExit
    ( TestList
        ( Unit.Placeholder.allTests
            ++ E2E.Placeholder.allTests
        )
    )
