module Day05.Mod where

import PseudoMacros (__FILE__)
import Utils.Intcode.Mod
import Utils.Mod

part1 :: IO ()
part1 = do
  memory <- getIntcodeInput (getInputFile $__FILE__)
  print $ last $ intcodeToList memory [1]
  return ()

part2 :: IO ()
part2 = do
  memory <- getIntcodeInput (getInputFile $__FILE__)
  print $ last $ intcodeToList memory [5]
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]