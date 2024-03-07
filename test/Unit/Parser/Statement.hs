module Unit.Parser.Statement (allTests) where

import AST
import Data.Either (fromRight, isRight)
import Parser.Statement
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple,
    TestLabel "functions" testFunctions
  ]

emptyTestStatement :: Statement
emptyTestStatement =
  VariableStatement
    ( Variable
        (VariableDeclaration "test" IntType)
        (AtomicExpression (LiteralAtomic (IntLiteral 0)))
    )

testSimple :: Test
testSimple = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseStatement "" ""))
  assertEqual
    "var defintion"
    (VariableStatement (Variable (VariableDeclaration "i" IntType) (AtomicExpression (LiteralAtomic (IntLiteral 1)))))
    (fromRight emptyTestStatement (parse parseStatement "" "int i = 1;"))
  assertEqual
    "var assignment literal number"
    (AssignmentStatement (Assignment "k" (AtomicExpression (LiteralAtomic (IntLiteral 2)))))
    (fromRight emptyTestStatement (parse parseStatement "" "k = 2;"))
  assertEqual
    "var assignment with var and number"
    ( AssignmentStatement
        ( Assignment
            "k"
            ( OperationExpression
                (AtomicExpression (VariableAtomic "k"))
                Multiply
                (AtomicExpression (LiteralAtomic (IntLiteral 1)))
            )
        )
    )
    (fromRight emptyTestStatement (parse parseStatement "" "k = k * 1;"))

emptyTestFunction :: Function
emptyTestFunction = Function "TEST" [] IntType []

testFunctions :: Test
testFunctions = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseFunction "" ""))
  assertEqual
    "empty main function"
    (Function "main" [] UnitType [])
    (fromRight emptyTestFunction (parse parseFunction "" "void main() { }"))
  assertEqual
    "main function"
    ( Function
        "main"
        []
        UnitType
        [ VariableStatement (Variable (VariableDeclaration "i" IntType) (AtomicExpression (LiteralAtomic (IntLiteral 1)))),
          AssignmentStatement (Assignment "i" (AtomicExpression (LiteralAtomic (IntLiteral 2))))
        ]
    )
    (fromRight emptyTestFunction (parse parseFunction "" "void main() { int i = 1; i = 2; }"))
  assertEqual
    "function with arguments"
    ( (Function "test" [VariableDeclaration "i" IntType, VariableDeclaration "k" IntType] FloatType [])
    )
    (fromRight emptyTestFunction (parse parseFunction "" "float test(int i, int k) { }"))
