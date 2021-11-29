module Day10.Mod where

import Data.List (maximumBy, sortOn, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import PseudoMacros (__FILE__)
import Utils.Cords
import Utils.Mod

part1 :: IO ()
part1 = do
  input <- readInputLines $ getInputFile $__FILE__
  let cords = [c | (c, '#') <- coordLines input]
  let byAngles = findTheBase cords
  print $ Map.size byAngles
  return ()

part2 :: IO ()
part2 = do
  input <- readInputLines $ getInputFile $__FILE__
  let cords = [c | (c, '#') <- coordLines input]
  let byAngles = findTheBase cords
  print $ spiralOrder byAngles
  let C y x = spiralOrder byAngles !! 199
  print (x * 100 + y)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

-- Find the best base location. Returns map of angles to asteroids it can see at that angle.
-- the list of asteroids are sorted by distance to the base
findTheBase :: [Coord] -> Map Double [Coord]
findTheBase world =
  maximumBy
    (comparing Map.size)
    [sortOn (manhattan i) <$> collectBy (cordAngle . subCoord i) xs | (i, xs) <- pickOne world]

-- | Given a characterizing function arrange elements that
-- have the same characterization.
collectBy :: Ord k => (a -> k) -> [a] -> Map k [a]
collectBy f xs = Map.fromListWith (++) [(f x, [x]) | x <- xs]

-- the transpose makes it so each every asteroid that would be hit on a rotation is in the same list
-- the concat will merge all the lists together so it would be in order of being hit
spiralOrder :: Map Double [Coord] -> [Coord]
spiralOrder = concat . transpose . Map.elems
