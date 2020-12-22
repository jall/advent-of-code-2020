module Main where

import Control.Monad (sequence_)
import Data.Maybe (fromMaybe)
import qualified Day_01
import qualified Day_02
import qualified Day_03
import qualified Day_04
import qualified Day_05
import qualified Day_06
import qualified Day_07
import qualified Day_08
import qualified Day_09
import qualified Day_10
import qualified Day_11
import qualified Day_12
import Utils (mapWithIndex)

main :: IO ()
main =
  sequence_ $
    map putStrLn $
      concat $
        mapWithIndex
          formatDay
          -- [ Day_01.solution,
          --   Day_02.solution,
          --   Day_03.solution,
          --   Day_04.solution,
          --   Day_05.solution,
          --   Day_06.solution,
          --   Day_07.solution,
          --   Day_08.solution,
          --   Day_09.solution,
          --   Day_10.solution,
          --   Day_11.solution,
          --   Day_12.solution
          -- ]
          [Day_12.solution]

formatDay :: (Maybe String, Maybe String) -> Int -> [String]
formatDay (part1, part2) index = do
  let fail = "No solution found"
  [ ("Day " ++ (show (index + 1))),
    ("a. " ++ (fromMaybe fail part1)),
    ("b. " ++ (fromMaybe fail part2)),
    ""
    ]
