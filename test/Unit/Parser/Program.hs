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
    "int k = 1;"
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
    "int k = 1; int j = 2;"
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
    "k = 1;"
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
    "int k = 1; j = 2;"
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
  assertEqual
    "print();"
    ( Program
        [ ExpressionStatement
            (AtomicExpression (FunctionCallAtomic "print" []))
        ]
    )
    (fromRight emptyProgram (parse parseProgram "" "print();"))
  assertEqual
    "function calls around function"
    ( Program
        [ ExpressionStatement
            (AtomicExpression (FunctionCallAtomic "test" [])),
          FunctionDefinitionStatement
            ( Function
                "test"
                []
                UnitType
                [ ExpressionStatement
                    ( AtomicExpression
                        ( FunctionCallAtomic
                            "print"
                            [AtomicExpression (LiteralAtomic (StringLiteral "Hello, World!"))]
                        )
                    )
                ]
            ),
          ExpressionStatement (AtomicExpression (FunctionCallAtomic "test" [])),
          ExpressionStatement (AtomicExpression (FunctionCallAtomic "test" []))
        ]
    )
    ( fromRight
        emptyProgram
        ( parse
            parseProgram
            ""
            "test();\n\
            \void test() {\n\
            \  print(\"Hello, World!\");\n\
            \}\n\
            \\n\
            \test();\n\
            \test();"
        )
    )
