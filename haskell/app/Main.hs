module Main where

-- need to update other-modules to add sub modules
import qualified Day01.Mod as Day01 (dispatch)
import qualified Day02.Mod as Day02 (dispatch)
import qualified Day03.Mod as Day03 (dispatch)
-- Add day import

import System.Environment (getArgs)

type DayDispatcher = [(Int, IO ())]

dayMap :: [(Int, DayDispatcher)]
dayMap =
  [ (1, Day01.dispatch),
    (2, Day02.dispatch),
    (3, Day03.dispatch)
    -- Add day dispatch
  ]

main :: IO ()
main = do
  [dayNumber, part] <- getArgs
  let (Just dayDispatcher) = lookup (read dayNumber) dayMap
  let (Just func) = lookup (read part) dayDispatcher
  func