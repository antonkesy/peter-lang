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
