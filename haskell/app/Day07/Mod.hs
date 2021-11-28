module Day07.Mod where

import Data.Function (fix)
import Data.List (permutations)
import PseudoMacros (__FILE__)
import Utils.Intcode.Mod
import Utils.Mod

-- | A function from a list of input values to a list of output values.
type ListFn = [Int] -> [Int]

-- | Given a amplifier controller software function and a list of
-- phase settings, generate the resulting list of thruster outputs.
--
-- Once instances of the control software is started for each phase setting,
-- the instances are all sequenced together into a single loop. A starting
-- @0@ element is added as an initial input.
--
-- >>> thrustController (intcodeToList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) [4,3,2,1,0]
-- [43210]
-- >>> thrustController (intcodeToList [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]) [9,8,7,6,5]
-- [129,4257,136353,4363425,139629729]
thrustController ::
  -- | amplifier controller software
  ListFn ->
  -- | thrust controller
  ListFn
thrustController ctrl phases = tieknot [ctrl << p | p <- phases]

-- | Create a feedback loop given the initialized controllers
-- and return the thruster outputs. Feed an initial @0@ value
-- into the loop.
--
-- >>> tieknot [map (2*), map (1+), take 5]
-- [1,3,7,15,31]
tieknot ::
  -- | initialized amplifier controllers
  [ListFn] ->
  -- | thruster outputs
  [Int]
tieknot fs = fix (composeLR fs << 0)

-- | Compose list functions from left-to-right. Inputs go into
-- first function and outputs come from last function.
--
-- >>> composeLR [(2*),(1+)] 3
-- 7
composeLR :: [a -> a] -> (a -> a)
composeLR = foldl (flip (.)) id

-- | Feed a single input into a list function.
--
-- >>> (map (*2) << 10) [5,6,7]
-- [20,10,12,14]
--
-- the above is equal to (map (*2)) [10, 5, 6, 7]
(<<) :: ListFn -> Int -> ListFn
(f << x) xs = f (x : xs)

optimize :: ([Int] -> Int) -> ListFn -> [Int] -> Int
optimize f pgm phaseVals = maximum [f $ thrustController pgm phase | phase <- permutations phaseVals]

part1 :: IO ()
part1 = do
  memory <- getIntcodeInput (getInputFile $__FILE__)
  print $ optimize head (intcodeToList memory) [0 .. 4]
  return ()

part2 :: IO ()
part2 = do
  memory <- getIntcodeInput (getInputFile $__FILE__)
  print $ optimize last (intcodeToList memory) [5 .. 9]
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
