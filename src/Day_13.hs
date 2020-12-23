module Day_13
  ( solution,
  )
where

import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Utils (mapWithIndex)

solution :: (Maybe String, Maybe String)
solution =
  let (earliestTime, rawBuses) = input
      buses = parseBuses rawBuses
   in ( Just . show $
          let (bus, time) = minimumBy (compare `on` snd) $ map (findFirstTimeAfter earliestTime . fst) buses
           in bus * (time - earliestTime),
        Just . show $ findSubsequentBusesTime buses 0 1
      )

findFirstTimeAfter :: Time -> ID -> (ID, Time)
findFirstTimeAfter earliestTime bus =
  let time = until (>= earliestTime) (+ bus) bus
   in (bus, time)

-- I cheated & am using this approach for this question
-- https://old.reddit.com/r/adventofcode/comments/kcb3bb/2020_day_13_part_2_can_anyone_tell_my_why_this/
findSubsequentBusesTime :: [(ID, Position)] -> Time -> Time -> Int
findSubsequentBusesTime [] currentTime _ = currentTime
findSubsequentBusesTime ((id, offset) : buses) currentTime period =
  if (currentTime + offset) `mod` id == 0
    then findSubsequentBusesTime buses currentTime (period * id)
    else findSubsequentBusesTime ((id, offset) : buses) (currentTime + period) period

parseBuses :: String -> [(ID, Position)]
parseBuses str =
  filter (\(id, _) -> id /= -1) $
    mapWithIndex (\s i -> (if s == "x" then -1 else read s, i)) $
      splitOn "," str

type ID = Int

type Time = Int

type Position = Int

testInput :: (Time, String)
testInput =
  (939, "7,13,x,x,59,x,31,19")

input :: (Time, String)
input =
  (1002561, "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,409,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,373,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19")
