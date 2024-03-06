module Unit.Parser.Assignment (allTests) where

import AST
import Data.Either (fromRight, isRight)
import Parser.Assignment
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple
  ]

emptyTestAssignment :: Assignment
emptyTestAssignment = (Assignment "test" (AtomicExpression (LiteralAtomic (IntLiteral 0))))

testSimple :: Test
testSimple = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseAssignment "" ""))
  assertEqual
    "Number Assignment"
    (Assignment "k" (AtomicExpression (LiteralAtomic (IntLiteral 2))))
    (fromRight emptyTestAssignment (parse parseAssignment "" "k = 2"))
  assertEqual
    "Variable Assignment"
    (Assignment "k" (AtomicExpression (VariableAtomic "k")))
    (fromRight emptyTestAssignment (parse parseAssignment "" "k = k"))
  assertEqual
    "Variable + Number Assignment"
    (Assignment "k" (OperationExpression (AtomicExpression (VariableAtomic "k")) Plus (AtomicExpression (LiteralAtomic (IntLiteral 1)))))
    (fromRight emptyTestAssignment (parse parseAssignment "" "k = k + 1"))
