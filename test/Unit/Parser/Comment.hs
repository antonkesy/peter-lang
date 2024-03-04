module Unit.Parser.Comment (allTests) where

import Data.Either (fromRight, isRight)
import Parser.Comment
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "testSingleLine" testSingleLineComment,
    TestLabel "testMultiLine" testMultiLineComment,
    TestLabel "both" testBothComments
  ]

testSingleLineComment :: Test
testSingleLineComment = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseSingleLineComment "" ""))
  assertEqual
    "too few '/'"
    False
    (isRight (parse parseSingleLineComment "" "/ comment"))
  assertEqual
    "Simple Comment"
    " comment"
    (fromRight "incorrect" (parse parseSingleLineComment "" "// comment"))
  assertEqual
    "No Space"
    "comment"
    (fromRight "incorrect" (parse parseSingleLineComment "" "//comment"))
  assertEqual
    "Trailing Space"
    "comment "
    (fromRight "incorrect" (parse parseSingleLineComment "" "//comment "))
  assertEqual
    "New Line End"
    "comment "
    (fromRight "incorrect" (parse parseSingleLineComment "" "//comment \n"))

testMultiLineComment :: Test
testMultiLineComment = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseMultiLineComment "" ""))
  assertEqual
    "too few '/'"
    False
    (isRight (parse parseMultiLineComment "" "/* comment"))
  assertEqual
    "Space between"
    " comment "
    (fromRight "incorrect" (parse parseMultiLineComment "" "/* comment */"))
  assertEqual
    "No Space"
    "comment"
    (fromRight "incorrect" (parse parseMultiLineComment "" "/*comment*/"))
  assertEqual
    "Multi Line Comment"
    " 1\n2 "
    (fromRight "incorrect" (parse parseMultiLineComment "" "/* 1\n2 */"))

testBothComments :: Test
testBothComments = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseComment "" ""))
  assertEqual
    "Single Line"
    " comment"
    (fromRight "incorrect" (parse parseComment "" "// comment"))
  assertEqual
    "Multi Line"
    " comment "
    (fromRight "incorrect" (parse parseComment "" "/* comment */"))
