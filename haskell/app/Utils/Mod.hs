module Utils.Mod where

import Data.List
import Data.Void
import System.FilePath (combine, takeDirectory)
import Text.Megaparsec (Parsec, anySingle, eof, manyTill, parse, satisfy, sepBy, setInput)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)

readInputLines :: FilePath -> IO [String]
readInputLines filename = lines <$> readFile filename

-- | Given a source file path, get the `in` file in that directory
getInputFile :: FilePath -> FilePath
getInputFile sourceFilePath = combine (takeDirectory sourceFilePath) "in"

-- | Given a source file path, get the `in.example` file in that directory
getExampleInputFile :: FilePath -> FilePath
getExampleInputFile sourceFilePath = combine (takeDirectory sourceFilePath) "in.example"

removeEmptyString :: [String] -> [String]
removeEmptyString = filter (not . null)

-- https://github.com/glguy/advent2019/blob/master/common/Advent.hs

type Parser = Parsec Void String

-- | Run a parser with 'parseLines' on the input file.
getParsedLines :: String -> Parser a -> [a]
getParsedLines input p =
  case parseLines p input of
    Left string -> error string
    Right res -> res

-- | Run a parser on each line of the input file. Each line will be parsed
-- in isolation. The parser must consume the whole line.
--
-- >>> parseLines (Control.Applicative.many anySingle) "12\n34\n"
-- Right ["12","34"]
-- >>> parseLines number "12\n34\n"
-- Right [12,34]
parseLines :: Parser a -> String -> Either String [a]
parseLines p input =
  case parse (traverse parse1 (lines input)) "input" input of
    Left e -> Left (errorBundlePretty e)
    Right a -> Right a
  where
    parse1 x = setInput x *> p <* eof <* setInput "\n" <* newline

-- | Parse a signed integral number
number :: Integral a => Parser a
number = signed (return ()) decimal

-- | Count the number of elements in a foldable value that satisfy a predicate.
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc + 1 else acc) 0