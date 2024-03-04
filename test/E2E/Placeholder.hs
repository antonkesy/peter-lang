module E2E.Placeholder (allTests) where

import Test.HUnit

allTests :: [Test]
allTests =
  [ TestLabel "testPlaceholder" testPlaceholder,
    TestLabel "testPlaceholder" testPlaceholder
  ]

testPlaceholder :: Test
testPlaceholder = TestCase $ do
  assertBool "Placholder" (True)
