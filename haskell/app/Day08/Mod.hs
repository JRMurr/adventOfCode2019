module Day08.Mod where

import Data.List
import Data.Maybe (isJust)
import PseudoMacros (__FILE__)
import Utils.Mod

chunk :: Int -> Int -> [a] -> [[a]]
chunk width height = chunks (width * height)

countChar :: Char -> String -> Int
countChar c = count (c ==)

findMinZeroLine :: [String] -> String
findMinZeroLine = minimumBy (\s1 s2 -> countZero s1 `compare` countZero s2)
  where
    countZero = countChar '0'

part1 :: IO ()
part1 = do
  [input] <- readInputLines $ getInputFile $__FILE__
  let layers = chunk 25 6 input
  let minLine = findMinZeroLine layers
  print $ countChar '1' minLine * countChar '2' minLine
  return ()

render :: Char -> Char
render '0' = '\x2591'
render '1' = '\x2588'
render '2' = '\x2592'
render _ = error "bad pixel"

overlayLayers :: [String] -> String
overlayLayers = foldr1 (zipWith overlay)

overlay :: Char -> Char -> Char
overlay '2' x = x
overlay x _ = x

part2 :: IO ()
part2 = do
  [input] <- readInputLines $ getInputFile $__FILE__
  let layers = chunk 25 6 input
  mapM_ (putStrLn . map render) (chunks 25 (overlayLayers layers))

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]