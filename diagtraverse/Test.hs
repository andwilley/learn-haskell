module Test where

import           Test.HUnit
import qualified Main

main :: IO ()
main = const (return ()) =<< runTestTT tests

in1 :: [[Int]]
in1 = [[1, 3, 4], [6, 2, 4], [7, 3, 5]]
out1 :: [Int]
out1 = [1, 6, 3, 7, 2, 4, 3, 4, 5]

in2 :: [[Int]]
in2 = [[1, 3, 4], [6, 2, 4], [2], [5], [3], [7, 3, 5]]
out2 :: [Int]
out2 = [1, 6, 3, 2, 2, 4, 5, 4, 3, 7, 3, 5]

test1 = TestCase $ assertEqual "should be equal"
                               out1
                               (Main.addItemsFromRows in1 ([[]], [[]]))


test2 = TestCase $ assertEqual "should be equal"
                               out2
                               (Main.addItemsFromRows in2 ([[]], [[]]))


tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
