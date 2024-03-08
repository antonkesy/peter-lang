import E2E.Placeholder
import Test.HUnit (Test (TestList), runTestTTAndExit)
import Unit.Parser.Assignment
import Unit.Parser.Atomic
import Unit.Parser.Comment
import Unit.Parser.Expression
import Unit.Parser.Program
import Unit.Parser.Statement

main :: IO ()
main =
  runTestTTAndExit
    ( TestList
        ( Unit.Parser.Assignment.allTests
            ++ Unit.Parser.Atomic.allTests
            -- ++ Unit.Parser.Comment.allTests
            ++ Unit.Parser.Expression.allTests
            ++ Unit.Parser.Program.allTests
            ++ Unit.Parser.Statement.allTests
            ++ E2E.Placeholder.allTests
        )
    )
