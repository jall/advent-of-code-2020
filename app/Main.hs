module Main where

import Data.Maybe (fromMaybe)
import qualified Day_01

main :: IO ()
main = putStrLn $ "Day 1: " ++ (fromMaybe "No solution found" Day_01.solution)
