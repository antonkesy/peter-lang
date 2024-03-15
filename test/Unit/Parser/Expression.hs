module Unit.Parser.Expression (allTests) where

import AST
import Data.Either (fromRight, isRight)
import Parser.Expression
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple
  ]

emptyTestExpression :: Expression
emptyTestExpression = AtomicExpression (LiteralAtomic (IntLiteral 0))

testSimple :: Test
testSimple = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseExpression "" ""))
  assertEqual
    "Single Number"
    (AtomicExpression (LiteralAtomic (IntLiteral 1)))
    (fromRight emptyTestExpression (parse parseExpression "" "1"))
  assertEqual
    "Function Call"
    (AtomicExpression (FunctionCallAtomic "print" []))
    (fromRight emptyTestExpression (parse parseExpression "" "print()"))
  assertEqual
    "(print())"
    (AtomicExpression (FunctionCallAtomic "print" []))
    (fromRight emptyTestExpression (parse parseExpression "" "(print())"))
  assertEqual
    "(1) + (3)"
    (OperationExpression (AtomicExpression (LiteralAtomic (IntLiteral 1))) Plus (AtomicExpression (LiteralAtomic (IntLiteral 3))))
    (fromRight emptyTestExpression (parse parseExpression "" "(1) + (3)"))
  assertEqual
    "1 + (3 + 4)"
    ( OperationExpression
        (AtomicExpression (LiteralAtomic (IntLiteral 1)))
        Plus
        ( OperationExpression
            (AtomicExpression (LiteralAtomic (IntLiteral 3)))
            Plus
            (AtomicExpression (LiteralAtomic (IntLiteral 4)))
        )
    )
    (fromRight emptyTestExpression (parse parseExpression "" "1 + (3 + 4)"))
  assertEqual
    "0 == (i % 3)"
    ( OperationExpression
        (AtomicExpression (LiteralAtomic (IntLiteral 0)))
        Eq
        ( OperationExpression
            (AtomicExpression (VariableAtomic "i"))
            Modulus
            (AtomicExpression (LiteralAtomic (IntLiteral 3)))
        )
    )
    (fromRight emptyTestExpression (parse parseExpression "" "0 == (i % 3)"))
