module Test where

import           Data.Vector
import qualified Data.Set                      as Set
import           Test.HUnit
import qualified Main

main :: IO ()
main = const (return ()) =<< runTestTT tests

layout1 :: Main.Layout Int
layout1 = fromList
  [ fromList [2, 0, 0, 0, 0, 0]
  , fromList [0, 0, 0, 0, 0, 0]
  , fromList [0, 1, 0, 0, 0, 0]
  , fromList [0, 0, 0, 0, 0, 0]
  , fromList [0, 0, 0, 2, 0, 0]
  , fromList [0, 0, 0, 0, 0, 0]
  ]

layout2 :: Main.Layout Int
layout2 = fromList
  [ fromList [2, 0, 0, 0, 0, 0]
  , fromList [0, 0, 0, 0, 0, 0]
  , fromList [0, 1, 0, 0, 2, 0]
  , fromList [0, 0, 0, 0, 0, 0]
  , fromList [0, 0, 0, 0, 0, 0]
  , fromList [0, 0, 1, 0, 0, 0]
  ]

layout3 :: Main.Layout Int
layout3 = fromList
  [ fromList [2, 1, 0, 1, 0, 0]
  , fromList [0, 1, 0, 0, 0, 0]
  , fromList [0, 1, 0, 1, 0, 1]
  , fromList [0, 1, 0, 1, 0, 0]
  , fromList [0, 1, 0, 1, 0, 1]
  , fromList [0, 0, 0, 0, 0, 0]
  ]

layout4 :: Main.Layout Int
layout4 = fromList
  [ fromList [2, 1, 0, 1, 0, 0]
  , fromList [0, 1, 0, 0, 0, 0]
  , fromList [0, 1, 0, 1, 0, 1]
  , fromList [0, 1, 0, 1, 0, 0]
  , fromList [1, 1, 0, 1, 0, 1]
  , fromList [0, 0, 0, 0, 0, 0]
  ]

p0_5 :: Main.Point Int Int
p0_5 = Main.Point 0 5

p1_5 :: Main.Point Int Int
p1_5 = Main.Point 1 5

p5_5 :: Main.Point Int Int
p5_5 = Main.Point 5 5

p5_0 :: Main.Point Int Int
p5_0 = Main.Point 5 0

p5_2 :: Main.Point Int Int
p5_2 = Main.Point 5 2

p2_5 :: Main.Point Int Int
p2_5 = Main.Point 2 5

p0_2 :: Main.Point Int Int
p0_2 = Main.Point 0 2

p2_2 :: Main.Point Int Int
p2_2 = Main.Point 2 2

p4_2 :: Main.Point Int Int
p4_2 = Main.Point 4 2

p2_4 :: Main.Point Int Int
p2_4 = Main.Point 2 4

pn1_0 :: Main.Point Int Int
pn1_0 = Main.Point (-1) 0

p0_0 :: Main.Point Int Int
p0_0 = Main.Point 0 0

p0_3 :: Main.Point Int Int
p0_3 = Main.Point 0 3

p4_3 :: Main.Point Int Int
p4_3 = Main.Point 4 3

p2_1 :: Main.Point Int Int
p2_1 = Main.Point 2 1

testGetInboundsIndexGetsVal1 =
  TestCase (assertEqual "should be 2" (Just 2) (Main.get p0_0 layout1))
testGetInboundsIndexGetsVal2 =
  TestCase (assertEqual "should be 1" (Just 1) (Main.get p2_1 layout1))
testGetOutOfBoundsIndexGetsNothing =
  TestCase (assertEqual "should be Nothing" Nothing (Main.get pn1_0 layout1))
testFoldToBorder = TestCase
  (assertEqual
    "should be Left 5, 2"
    (Left p5_2)
    (Main.foldLayoutByStepFromWithUntil (Main.addPoints Main.down)
                                        p0_2
                                        Main.goalOrNode
                                        (Right p0_2)
                                        Main.stopAt1or2
                                        layout1
    )
  )
testFoldToWall = TestCase
  (assertEqual
    "should be Left 2, 2"
    (Left p2_2)
    (Main.foldLayoutByStepFromWithUntil (Main.addPoints Main.left)
                                        p2_5
                                        Main.goalOrNode
                                        (Right p2_5)
                                        Main.stopAt1or2
                                        layout1
    )
  )
testFoldToGoal = TestCase
  (assertEqual
    "should be Right 4, 3"
    (Right p4_3)
    (Main.foldLayoutByStepFromWithUntil (Main.addPoints Main.down)
                                        p0_3
                                        Main.goalOrNode
                                        (Left p4_3)
                                        Main.stopAt1or2
                                        layout1
    )
  )
testGetChildNodes = TestCase
  (assertEqual "should be find all the moves"
               [Left p0_2, Left p4_2, Right p2_4]
               (Main.getChildNodes layout2 p2_2 Main.allMoves)
  )
testGetChildNodes2 = TestCase
  (assertEqual "should be find all the moves"
               [Left p5_0]
               (Main.getChildNodes layout3 p5_5 Main.allMoves)
  )
testGetChildNodes3 = TestCase
  (assertEqual "should be find all the moves"
               [Right p0_0, Left p5_5]
               (Main.getChildNodes layout3 p5_0 Main.allMoves)
  )
testBfsGoalFound = TestCase
  (assertEqual
    "should find the goal"
    True
    (Main.bfs layout3 (singleton (Left p0_5)) (Set.fromList []) (Left p0_5))
  )
testBfsGoalFound2 = TestCase
  (assertEqual
    "should find the goal"
    True
    (Main.bfs layout3 (singleton (Left p5_5)) (Set.fromList []) (Left p5_5))
  )
testBfsGoalFound3 = TestCase
  (assertEqual
    "should find the goal"
    True
    (Main.bfs layout3 (singleton (Left p5_0)) (Set.fromList []) (Left p5_0))
  )
testBfsNoGoalFound = TestCase
  (assertEqual
    "should find the goal"
    False
    (Main.bfs layout4 (singleton (Left p1_5)) (Set.fromList []) (Left p1_5))
  )

tests = TestList
  [ TestLabel "get exists 0 0"    testGetInboundsIndexGetsVal1
  , TestLabel "get exists 2 1"    testGetInboundsIndexGetsVal2
  , TestLabel "get not exists"    testGetOutOfBoundsIndexGetsNothing
  , TestLabel "fold to border"    testFoldToBorder
  , TestLabel "fold to wall"      testFoldToWall
  , TestLabel "fold to goal"      testFoldToGoal
  , TestLabel "get child nodes"   testGetChildNodes
  , TestLabel "get child nodes2"  testGetChildNodes2
  , TestLabel "get child nodes3"  testGetChildNodes3
  , TestLabel "bfs goal found"    testBfsGoalFound
  , TestLabel "bfs goal found2"   testBfsGoalFound2
  , TestLabel "bfs goal found3"   testBfsGoalFound3
  , TestLabel "bfs no goal found" testBfsNoGoalFound
  ]
