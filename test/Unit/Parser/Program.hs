module Unit.Parser.Program (allTests) where

import AST
import Data.Either (fromRight, isRight)
import Parser.Program
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple
  ]

emptyProgram :: Program
emptyProgram = Program []

testSimple :: Test
testSimple = TestCase $ do
  assertEqual
    "empty"
    True
    (isRight (parse parseProgram "" ""))
  assertEqual
    "Single variable statement"
    ( Program
        [ VariableStatement
            ( Variable
                (VariableDeclaration "k" IntType)
                ( AtomicExpression (LiteralAtomic (IntLiteral 1))
                )
            )
        ]
    )
    (fromRight emptyProgram (parse parseProgram "" "int k = 1;"))
  assertEqual
    "Multiple variable statements"
    ( Program
        [ VariableStatement
            ( Variable
                (VariableDeclaration "k" IntType)
                ( AtomicExpression (LiteralAtomic (IntLiteral 1))
                )
            ),
          VariableStatement
            ( Variable
                (VariableDeclaration "j" IntType)
                ( AtomicExpression (LiteralAtomic (IntLiteral 2))
                )
            )
        ]
    )
    (fromRight emptyProgram (parse parseProgram "" "int k = 1; int j = 2;"))
  assertEqual
    "Single assignment statement"
    ( Program
        [ AssignmentStatement
            ( Assignment
                "k"
                ( AtomicExpression (LiteralAtomic (IntLiteral 1))
                )
            )
        ]
    )
    (fromRight emptyProgram (parse parseProgram "" "k = 1;"))
  assertEqual
    "variable statement and assignment statement"
    ( Program
        [ VariableStatement
            ( Variable
                (VariableDeclaration "k" IntType)
                ( AtomicExpression (LiteralAtomic (IntLiteral 1))
                )
            ),
          AssignmentStatement
            ( Assignment
                "j"
                ( AtomicExpression (LiteralAtomic (IntLiteral 2))
                )
            )
        ]
    )
    (fromRight emptyProgram (parse parseProgram "" "int k = 1; j = 2;"))
