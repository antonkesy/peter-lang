module Unit.Parser.Statement (allTests) where

import AST
import Data.Either (fromRight, isRight)
import Parser.Statement
import Parser.Struct
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple,
    TestLabel "functions" testFunctions,
    TestLabel "return" testReturn,
    TestLabel "if" testIf,
    TestLabel "while" testWhile,
    TestLabel "struct" testStruct
  ]

emptyTestStatement :: Statement
emptyTestStatement =
  VariableDefinitionStatement
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
    "int i = 1;"
    (VariableDefinitionStatement (Variable (VariableDeclaration "i" IntType) (AtomicExpression (LiteralAtomic (IntLiteral 1)))))
    (fromRight emptyTestStatement (parse parseStatement "" "int i = 1;"))
  assertEqual
    "k = 2;"
    (AssignmentStatement (Assignment "k" (AtomicExpression (LiteralAtomic (IntLiteral 2)))))
    (fromRight emptyTestStatement (parse parseStatement "" "k = 2;"))
  assertEqual
    "k = k * 1;"
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
  assertEqual
    "print();"
    (ExpressionStatement (AtomicExpression (FunctionCallAtomic "print" [])))
    (fromRight emptyTestStatement (parse parseStatement "" "print();"))

emptyTestFunction :: Function
emptyTestFunction = Function "TEST" [] IntType []

testFunctions :: Test
testFunctions = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseFunction "" ""))
  assertEqual
    "void main() { }"
    (Function "main" [] UnitType [])
    (fromRight emptyTestFunction (parse parseFunction "" "void main() { }"))
  assertEqual
    "void main() { int i = 1; i = 2; }"
    ( Function
        "main"
        []
        UnitType
        [ VariableDefinitionStatement (Variable (VariableDeclaration "i" IntType) (AtomicExpression (LiteralAtomic (IntLiteral 1)))),
          AssignmentStatement (Assignment "i" (AtomicExpression (LiteralAtomic (IntLiteral 2))))
        ]
    )
    (fromRight emptyTestFunction (parse parseFunction "" "void main() { int i = 1; i = 2; }"))
  assertEqual
    "float test(int i, int k) { }"
    ( Function "test" [VariableDeclaration "i" IntType, VariableDeclaration "k" IntType] FloatType []
    )
    (fromRight emptyTestFunction (parse parseFunction "" "float test(int i, int k) { }"))

testReturn :: Test
testReturn = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseReturnStatement "" ""))
  assertEqual
    "return;"
    (ReturnStatement (AtomicExpression (LiteralAtomic UnitLiteral)))
    (fromRight emptyTestStatement (parse parseReturnStatement "" "return;"))
  assertEqual
    "return; -> statement"
    (fromRight emptyTestStatement (parse parseReturnStatement "" "return;"))
    (fromRight emptyTestStatement (parse parseStatement "" "return;"))
  assertEqual
    "return 1;"
    (ReturnStatement (AtomicExpression (LiteralAtomic (IntLiteral 1))))
    (fromRight emptyTestStatement (parse parseReturnStatement "" "return 1;"))
  assertEqual
    "return 1; -> statement"
    (fromRight emptyTestStatement (parse parseReturnStatement "" "return 1;"))
    (fromRight emptyTestStatement (parse parseStatement "" "return 1;"))

testIf :: Test
testIf = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseControl "" ""))
  assertEqual
    "if true {}"
    (ControlStatement (IfControl (AtomicExpression (LiteralAtomic (BoolLiteral True))) [] Nothing))
    (either (const emptyTestStatement) ControlStatement (parse parseControl "" "if true {}"))
  assertEqual
    "if true {} -> statement"
    (either (const emptyTestStatement) ControlStatement (parse parseControl "" "if true {}"))
    (fromRight emptyTestStatement (parse parseStatement "" "if true {}"))
  assertEqual
    "if true { return 0; } else { return 1; }"
    ( ControlStatement
        ( IfControl
            (AtomicExpression (LiteralAtomic (BoolLiteral True)))
            [ReturnStatement (AtomicExpression (LiteralAtomic (IntLiteral 0)))]
            (Just [ReturnStatement (AtomicExpression (LiteralAtomic (IntLiteral 1)))])
        )
    )
    (either (const emptyTestStatement) ControlStatement (parse parseControl "" "if true { return 0; } else { return 1; }"))

testWhile :: Test
testWhile = TestCase $ do
  assertEqual
    "while true {}"
    (ControlStatement (WhileControl (AtomicExpression (LiteralAtomic (BoolLiteral True))) []))
    (either (const emptyTestStatement) ControlStatement (parse parseControl "" "while true {}"))
  assertEqual
    "while true { return 0; }"
    ( ControlStatement
        ( WhileControl
            (AtomicExpression (LiteralAtomic (BoolLiteral True)))
            [ReturnStatement (AtomicExpression (LiteralAtomic (IntLiteral 0)))]
        )
    )
    (either (const emptyTestStatement) ControlStatement (parse parseControl "" "while true { return 0; }"))

testStruct :: Test
testStruct = TestCase $ do
  assertEqual
    "struct Name {}"
    (Struct "Name" [])
    (fromRight (Struct "DEFAULT" []) (parse parseStruct "" "struct Name {}"))
  assertEqual
    "struct Name {}"
    (either (const emptyTestStatement) StructStatement (parse parseStruct "" "struct Name {}"))
    (fromRight emptyTestStatement (parse parseStatement "" "struct Name {}"))
  assertEqual
    "struct Name {\
    \  int x;\
    \  str n;\
    \  Name next;\
    \}"
    (Struct "Name" [VariableDeclaration "x" IntType, VariableDeclaration "n" StringType, VariableDeclaration "next" (CustomType "Name")])
    ( fromRight
        (Struct "DEFAULT" [])
        ( parse
            parseStruct
            ""
            "struct Name {\
            \  int x;\
            \  str n;\
            \  Name next;\
            \}"
        )
    )
