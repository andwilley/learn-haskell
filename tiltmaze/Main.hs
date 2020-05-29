module Main where

import           Data.Bifunctor
import qualified Data.Vector                   as Vector
import qualified Data.Set                      as Set
import           Data.Either
import           Data.Maybe

(+++) :: Vector.Vector a -> Vector.Vector a -> Vector.Vector a
a +++ b = a Vector.++ b

main :: IO ()
main = print "test"

type Layout a = Vector.Vector (Vector.Vector a)

data Point a b = Point { row :: a
                       , col :: b
                       } deriving (Eq, Show, Ord)
instance Bifunctor Point where
  bimap f g p = Point { row = f (row p), col = g (col p) }

either' :: (a -> b) -> Either a a -> b
either' f = either f f

extract :: Either a a -> a
extract = either' id

type Move = Point Int Int

addPoints :: Num a => Point a a -> Point a a -> Point a a
addPoints p = bimap (+ row p) (+ col p)

-- given point, return the Maybe value at that 2D index
get :: Point Int Int -> Layout a -> Maybe a
get (Point r c) l = (Vector.!? c) =<< l Vector.!? r

-- given a step, a function to perform at each step, and a predicate,
-- navigate the graph by the step until the predicate is True or the end of
-- the structure
foldLayoutByStepFromWithUntil
  :: (Point Int Int -> Point Int Int)
  -> Point Int Int
  -> (Point Int Int -> b -> a -> b)
  -> b
  -> (a -> Bool)
  -> Layout a
  -> b
foldLayoutByStepFromWithUntil step p f accum pred l = case val of
  Just x -> if stop
    -- if pred True, return
    then res
    -- preform f
    -- run again at new step
    else foldLayoutByStepFromWithUntil step (step p) f res pred l
  -- if get nothing, we're outside the graph, return
  Nothing -> res
 where
  val  = get p l
  res  = maybe accum (f p accum) val
  stop = maybe True pred val

foldLayoutBySingleStep
  :: Point Int Int
  -> (Point Int Int -> b -> a -> b)
  -> b
  -> (a -> Bool)
  -> Layout a
  -> Move
  -> b
foldLayoutBySingleStep a b c d e move =
  foldLayoutByStepFromWithUntil (addPoints move) a b c d e

-- Moves
up :: Move
up = Point (-1) 0

down :: Move
down = Point 1 0

left :: Move
left = Point 0 (-1)

right :: Move
right = Point 0 1

allMoves :: [Move]
allMoves = [up, down, left, right]

-- accumulator, if its a goal, return left point, otherwise return right point
-- edge case where the returned node is the same as the start
-- just filter it when we gen the moves
goalOrNode
  :: Point Int Int
  -> Either (Point Int Int) (Point Int Int)
  -> Int
  -> Either (Point Int Int) (Point Int Int)
goalOrNode pt accum val | val == 2  = Right pt
                        | val == 1  = accum
                        | otherwise = Left pt

-- stop at the goal or a wall
stopAt1or2 :: Int -> Bool
stopAt1or2 val = val == 2 || val == 1

-- generate list of child nodes
getChildNodes
  :: Layout Int
  -> Point Int Int
  -> [Move]
  -> [Either (Point Int Int) (Point Int Int)]
getChildNodes l p = Prelude.filter (either' (/= p))
  . fmap (foldLayoutBySingleStep p goalOrNode (Left p) stopAt1or2 l)

notInSets
  :: Set.Set (Point Int Int)
  -> Vector.Vector (Either (Point Int Int) (Point Int Int))
  -> Either (Point Int Int) (Point Int Int)
  -> Bool
notInSets s v ep = Set.notMember p s
  && isNothing (Vector.find ((== p) . extract) v)
  where p = extract ep

-- bfs
bfs
  :: Layout Int
  -- queue
  -> Vector.Vector (Either (Point Int Int) (Point Int Int))
  -- seen
  -> Set.Set (Point Int Int)
  -- next point
  -> Either (Point Int Int) (Point Int Int)
  -> Bool
bfs l queue seen p
  | Vector.null queue            = False
  | isRight next                 = True
  | not (null $ rights children) = True
  | otherwise = bfs l rest (Set.insert (either' id next) seen) next
 where
  -- pop the next node
  next     = Vector.head queue
  qTail    = Vector.tail queue
  children = getChildNodes l (extract p) allMoves
  -- append children not in seen or queue to the queue
  rest     = qTail +++ Vector.fromList (filter (notInSets seen qTail) children)
