module Test where

import           Test.HUnit
import qualified Main

main :: IO ()
main = const (return ()) =<< runTestTT tests

test1 = TestCase (assertEqual "should be equal" "abc" (Main.rev "cba"))
test2 = TestCase (assertEqual "fails" 3 4)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
