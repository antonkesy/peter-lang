module Unit.Parser.Atomic (allTests) where

import AST
import Data.Either (fromRight, isRight)
import Parser.Atomic
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple
  ]

emptyTestAtomic :: Atomic
emptyTestAtomic = (LiteralAtomic (UnitLiteral))

testSimple :: Test
testSimple = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseAtomic "" ""))
  assertEqual
    "Single Number"
    (LiteralAtomic (IntLiteral 1))
    (fromRight emptyTestAtomic (parse parseAtomic "" "1"))
  assertEqual
    "Function Call"
    (FunctionCallAtomic "print" [])
    (fromRight emptyTestAtomic (parse parseAtomic "" "print()"))
