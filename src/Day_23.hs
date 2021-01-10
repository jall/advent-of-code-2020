module Day_23 (solution) where

import Data.List.Split (splitOn)

solution :: (Maybe String, Maybe String)
solution =
  let start = input
   in ( Just $
          let moves = iterate move start
           in foldl1 (++) $ map show $ after 1 $ moves !! 100,
        Nothing
      )

move :: [Cup] -> [Cup]
move (current : cups) =
  let (nextThree, remainder) = splitAt 3 cups
      destination = findDestinationCup (current - 1) remainder
      chunks = splitOn [destination] remainder
      preceeding = head chunks
      following = last chunks
   in preceeding ++ destination : nextThree ++ following ++ [current]

findDestinationCup :: Cup -> [Cup] -> Cup
findDestinationCup current cups =
  let lowest = minimum cups
      highest = maximum cups
      next = if current - 1 < lowest then highest else current - 1
   in if current `elem` cups
        then current
        else findDestinationCup next cups

after :: Cup -> [Cup] -> [Cup]
after cup cups =
  let chunks = splitOn [cup] cups
      preceeding = head chunks
      following = last chunks
   in following ++ preceeding

type Cup = Integer

testInput :: [Cup]
testInput = [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: [Cup]
input = [4, 6, 7, 5, 2, 8, 1, 9, 3]
