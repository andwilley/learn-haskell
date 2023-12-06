main :: IO ()
main = const (return ()) =<< runTestTT tests

test1 =
  TestCase
    (assertEqual "should be equal" Zero (Main.checkTrie "cba" Main.numTrie))

test2 =
  TestCase
    (assertEqual "should be equal" Partial (Main.checkTrie "on" Main.numTrie))

test3 =
  TestCase
    (assertEqual "should be equal" Full (Main.checkTrie "nine" Main.numTrie))

tests =
  TestList
    [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]
