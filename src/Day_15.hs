module Day_15
  ( solution,
  )
where

import Data.List (elemIndices)
import qualified Data.Map
import Debug.Trace (trace)

solution :: (Maybe String, Maybe String)
solution =
  let initial = input
      initialMemoryTuples = zip initial (zip [1 ..] (repeat 0))
      starterRounds =
        map
          ( \(value, (turn, _)) ->
              (turn, value, Data.Map.fromList $ take turn initialMemoryTuples)
          )
          initialMemoryTuples
      starterRound =
        let turn = length initial
            value = last initial
            memory = Data.Map.fromList initialMemoryTuples
         in (turn + 1, value, memory)
      rounds = starterRounds ++ iterate nextRound starterRound
   in ( Just . show $ let (_, finalNumber, _) = rounds !! 2020 in finalNumber,
        Just . show $ let (_, finalNumber, _) = rounds !! 30000000 in finalNumber
      )

nextRound :: Round -> Round
nextRound (turn, value, memory) =
  let nextValue = case getPreviousTurns memory value of
        -- Yes yes this is ridiculously hacky & I'm only doing it because I have three valid cases I need to handle & don't want nested Maybes/Eithers
        (0, 0) -> 0
        (previousTurn, secondToLastTurn) -> if secondToLastTurn /= 0 then previousTurn - secondToLastTurn else 0
      nextMemory = updateMemory memory (turn, nextValue)
   in (turn + 1, nextValue, nextMemory)

updateMemory :: Memory -> (Turn, Value) -> Memory
updateMemory memory (turn, value) =
  let (previousTurn, _) = getPreviousTurns memory value
   in Data.Map.insert value (turn, previousTurn) memory

getPreviousTurns :: Memory -> Value -> (Turn, Turn)
getPreviousTurns memory value = case Data.Map.lookup value memory of
  Nothing -> (0, 0)
  Just (previousTurn, secondToLastTurn) -> (previousTurn, secondToLastTurn)

type Round = (Turn, Value, Memory)

type Turn = Int

type Value = Int

type Memory = Data.Map.Map Value (Turn, Turn)

testInput :: [Int]
testInput = [0, 3, 6]

input :: [Int]
input = [0, 1, 5, 10, 3, 12, 19]
