module Main where

import Control.Monad (sequence_)
import Data.Maybe (fromMaybe)
import qualified Day_01

main :: IO ()
main = sequence_ $ map putStrLn $ concat $ mapWithIndex formatDay [Day_01.solution]

formatDay :: (Maybe String, Maybe String) -> Int -> [String]
formatDay (part1, part2) index = do
  let fail = "No solution found"
  [ ("Day " ++ (show (index + 1))),
    ("a. " ++ (fromMaybe fail part1)),
    ("b. " ++ (fromMaybe fail part2)),
    ""
    ]

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0 ..]
