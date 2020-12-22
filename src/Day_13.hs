module Day_13
  ( solution,
  )
where

import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOn)

solution :: (Maybe String, Maybe String)
solution =
  let (earliestTime, rawBuses) = input
      buses = parseBuses rawBuses
   in ( Just . show $
          let (bus, time) = minimumBy (compare `on` snd) $ map (findFirstTimeAfter earliestTime) buses
           in bus * (time - earliestTime),
        Nothing
      )

findFirstTimeAfter :: Int -> Int -> (Int, Int)
findFirstTimeAfter earliestTime bus =
  let time = until (>= earliestTime) (+ bus) bus
   in (bus, time)

parseBuses :: String -> [Int]
parseBuses s = map read $ filter (/= "x") $ splitOn "," s

testInput =
  (939, "7,13,x,x,59,x,31,19")

input =
  (1002561, "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,409,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,373,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19")
