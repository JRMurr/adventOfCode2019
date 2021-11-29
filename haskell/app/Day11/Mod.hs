{-# LANGUAGE ImportQualifiedPost #-}

module Day11.Mod where

import Data.Map (Map)
import Data.Map qualified as Map
import PseudoMacros (__FILE__)
import Utils.Cords (Coord, addCoord, drawCoords, north, origin, turnLeft, turnRight)
import Utils.Intcode.Mod (getIntcodeInput, intcodeToList)
import Utils.Mod

runner ::
  -- | intcode program
  [Int] ->
  -- | initial world
  Map Coord Int ->
  -- | final world
  Map Coord Int
runner inp world0 = last [w | (_, _, w) <- states]
  where
    inputs = [Map.findWithDefault 0 here world | (_, here, world) <- states]
    outputs = intcodeToList inp inputs
    states = (north, origin, world0) : zipWith robotStep states (pairs outputs)

part1 :: IO ()
part1 = do
  memory <- getIntcodeInput (getInputFile $__FILE__)
  let world = runner memory Map.empty
  print $ Map.size world
  return ()

part2 :: IO ()
part2 = do
  memory <- getIntcodeInput (getInputFile $__FILE__)
  let render = putStrLn . drawCoords . fmap paintChar
  let world = runner memory (Map.singleton origin 1)
  render world
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

-- | Apply the robot movement logic to the current robot state
robotStep ::
  -- | vector, location, world
  (Coord, Coord, Map Coord Int) ->
  -- | robot's command
  (Int, Int) ->
  -- | vector, location, world
  (Coord, Coord, Map Coord Int)
robotStep (dir, here, world) (color, turn) = (dir', here', world')
  where
    world' = Map.insert here color world
    dir' = turnFn turn dir
    here' = addCoord here dir'

-- | Compute the turn function given a robot's output.
turnFn ::
  -- | robot turn output
  Int ->
  Coord ->
  Coord
turnFn 0 = turnLeft
turnFn 1 = turnRight
turnFn x = error ("Unexpected turn command: " ++ show x)

-- | Character representation of paint number.
paintChar :: Int -> Char
paintChar 0 = '░'
paintChar 1 = '█'
paintChar x = error ("Unexpected paint color: " ++ show x)

pairs :: [a] -> [(a, a)]
pairs (x : y : z) = (x, y) : pairs z
pairs _ = []