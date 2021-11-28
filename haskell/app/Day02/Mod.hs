module Day02.Mod where

import Data.List.Split
import qualified Data.Vector.Unboxed as V
import PseudoMacros (__FILE__)
import Utils.Intcode.Mod
import Utils.Mod (getExampleInputFile, getInputFile, readInputLines, removeEmptyString)

getMachine :: IO Machine
getMachine = new <$> getIntcodeInput (getInputFile $__FILE__)

runPgm :: Machine -> Machine
runPgm mach =
  case step mach of
    Step mach' -> runPgm mach'
    StepHalt -> mach
    _ -> error "Unexpected step"

-- | Run the given program after assigning the given noun and verb.
startup ::
  -- | noun
  Int ->
  -- | verb
  Int ->
  Machine ->
  Int
startup noun verb =
  (! 0)
    . runPgm
    . set 1 noun
    . set 2 verb

part1 :: IO ()
part1 = do
  pgm <- getMachine
  print (startup 12 2 pgm)
  return ()

part2 :: IO ()
part2 = do
  pgm <- getMachine
  print
    ( head
        [ 100 * noun + verb
          | noun <- [0 .. 99],
            verb <- [0 .. 99],
            startup noun verb pgm == 19690720
        ]
    )
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]