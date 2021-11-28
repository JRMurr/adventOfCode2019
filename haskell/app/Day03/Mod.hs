module Day03.Mod where

import Data.List (intersect, minimumBy)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import PseudoMacros (__FILE__)
import Utils.Mod (getExampleInputFile, getInputFile, readInputLines)

data TurnDir = U | D | L | R deriving (Show)

data Turn = Turn {dir :: TurnDir, amount :: Int}

type Path = [Turn]

newtype Point = Point (Int, Int, Int) deriving (Show)

instance Eq Point where
  Point (x1, y1, _) == Point (x2, y2, _) = (x1, y1) == (x2, y2)

instance Ord Point where
  Point (x1, y1, _) `compare` Point (x2, y2, _) = (x1, y1) `compare` (x2, y2)

data Wire = Wire {points :: Set Point, currentPoint :: Point} deriving (Show)

-- | Split single turn str to Turn
toTurn :: String -> Turn
toTurn ('U' : xs) = Turn {dir = U, amount = read xs}
toTurn ('D' : xs) = Turn {dir = D, amount = read xs}
toTurn ('L' : xs) = Turn {dir = L, amount = read xs}
toTurn ('R' : xs) = Turn {dir = R, amount = read xs}

-- | Split string containing ',' sep Turns to list of turn
parsePath :: String -> Path
parsePath str = map toTurn (splitOn "," str)

traverseTurn :: Point -> TurnDir -> Point
traverseTurn (Point (x, y, length)) U = Point (x, y + 1, length + 1)
traverseTurn (Point (x, y, length)) D = Point (x, y - 1, length + 1)
traverseTurn (Point (x, y, length)) L = Point (x - 1, y, length + 1)
traverseTurn (Point (x, y, length)) R = Point (x + 1, y, length + 1)

traverseWire :: Wire -> Turn -> Wire
traverseWire wire Turn {amount = 0} = wire
traverseWire Wire {currentPoint = curr, points = points} Turn {dir = dir, amount = amount} =
  let newPos = traverseTurn curr dir
   in traverseWire Wire {currentPoint = newPos, points = Set.insert newPos points} Turn {dir = dir, amount = amount -1}

pathToPoints :: Wire -> Path -> Wire
pathToPoints wire [] = wire
pathToPoints w (turn : turns) = pathToPoints (traverseWire w turn) turns

getWire :: Path -> Wire
getWire = pathToPoints Wire {currentPoint = Point (0, 0, 0), points = Set.empty}

getDistancePart1 :: Point -> Int
getDistancePart1 (Point (x, y, _)) = abs x + abs y

getClosestPoint :: (Point -> Int) -> (Set Point, Set Point) -> Point
getClosestPoint distFunc (p1, p2) = minimumBy (\p1 p2 -> distFunc p1 `compare` distFunc p2) common
  where
    common = p1 `Set.intersection` p2

getDistancePart2 :: Point -> Int
getDistancePart2 (Point (_, _, len)) = len

getMinDistPart2 :: (Set Point, Set Point) -> Int
getMinDistPart2 (p1, p2) =
  minimum $ zipWith (+) distance1 distance2
  where
    -- Set.intersection returns the elems of the first list so need to do it for both since the eq check does not look at length
    distance1 = map getDistancePart2 $ Set.toList $ p1 `Set.intersection` p2
    distance2 = map getDistancePart2 $ Set.toList $ p2 `Set.intersection` p1

part1 :: IO ()
part1 = do
  input <- readInputLines $ getInputFile $__FILE__
  let [w1, w2] = map (getWire . parsePath) input
  print (points w1 `Set.intersection` points w2)
  let p = getClosestPoint getDistancePart1 (points w1, points w2)
  print p
  print $ getDistancePart1 p
  return ()

part2 :: IO ()
part2 = do
  input <- readInputLines $ getInputFile $__FILE__
  let [w1, w2] = map (getWire . parsePath) input
  print $ getMinDistPart2 (points w1, points w2)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]