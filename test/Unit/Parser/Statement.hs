module Unit.Parser.Statement (allTests) where

import AST
import Data.Either (fromRight, isRight)
import Parser.Statement
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple
  ]

emptyTestStatement :: Statement
emptyTestStatement = VariableStatement (Variable "test" IntType (AtomicExpression (LiteralAtomic (IntLiteral 0))))

testSimple :: Test
testSimple = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseStatement "" ""))
  assertEqual
    "var defintion"
    (VariableStatement (Variable "i" IntType (AtomicExpression (LiteralAtomic (IntLiteral 1)))))
    (fromRight emptyTestStatement (parse parseStatement "" "int i = 1;"))
  assertEqual
    "var assignment literal number"
    (AssignmentStatement (Assignment "k" (AtomicExpression (LiteralAtomic (IntLiteral 2)))))
    (fromRight emptyTestStatement (parse parseStatement "" "k = 2;"))
  assertEqual
    "var assignment with var and number"
    ( AssignmentStatement
        ( ( Assignment
              "k"
              ( OperationExpression
                  (AtomicExpression (VariableAtomic "k"))
                  Multiply
                  (AtomicExpression (LiteralAtomic (IntLiteral 1)))
              )
          )
        )
    )
    (fromRight emptyTestStatement (parse parseStatement "" "k = k * 1;"))
