module Day01.Mod where

import PseudoMacros (__FILE__)
import Utils.Mod (getInputFile, readInput)

getFuel :: Integer -> Integer
getFuel mass = (mass `div` 3) - 2

getFuelRec :: Integer -> Integer
getFuelRec mass
  | needed >= 9 = needed + getFuelRec needed
  | otherwise = needed
  where
    needed = getFuel mass

part1 :: IO ()
part1 = do
  input <- readInput $ getInputFile $__FILE__
  print $ sum (map (getFuel . read) input)
  return ()

part2 :: IO ()
part2 = do
  input <- readInput $ getInputFile $__FILE__
  print $ sum (map (getFuelRec . read) input)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]