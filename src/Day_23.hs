module Day_23 (solution) where

import Control.Monad
import Control.Monad.ST
import Data.List.Split (splitOn)
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V

-- Part 2 requires a Mutable Vector & the ST monad
-- Excellent walkthrough of the vector tricks in Part 2
-- https://work.njae.me.uk/2021/01/08/advent-of-code-2020-day-23/

solution :: (Maybe String, Maybe String)
solution =
  let start = input
   in ( Just $
          let numberOfCups = 9
              numberOfRounds = 100
              finalState = game start numberOfCups numberOfRounds
           in foldl1 (++) $ map show $ drop 1 $ scanl (\x _ -> finalState !! x) 1 [1 .. (numberOfCups - 1)],
        Just . show $
          let numberOfCups = 1000000
              numberOfRounds = 10000000
              finalState = game start numberOfCups numberOfRounds
              a = finalState !! 1
              b = finalState !! a
           in a * b
      )

game :: [Cup] -> Int -> Int -> [Int]
game initial numberOfCups numberOfRounds =
  runST $
    do
      cups <- seed initial numberOfCups
      forM_
        [1 .. numberOfRounds]
        (\_ -> move cups numberOfCups)
      mapM (V.read cups) [0 .. numberOfCups]

move :: V.MVector s Int -> Int -> ST s ()
move cups highest =
  do
    current <- V.read cups 0

    held1 <- V.read cups current
    held2 <- V.read cups held1
    held3 <- V.read cups held2

    -- Remove the held cups from the list
    afterHeld <- V.read cups held3
    V.write cups current afterHeld

    let destination = findDestination (current - 1) highest [held1, held2, held3]
    afterDestination <- V.read cups destination

    -- Stitch the held cups into their destination
    -- They're still linked to each other, so only the beginning & end need updating
    V.write cups destination held1
    V.write cups held3 afterDestination

    -- Move current to the next item
    next <- V.read cups current
    V.write cups 0 next

    return ()

findDestination :: Cup -> Cup -> [Cup] -> Cup
findDestination 0 highest toSkip = findDestination highest highest toSkip
findDestination current highest toSkip
  | current `elem` toSkip = findDestination (current - 1) highest toSkip
  | otherwise = current

seed :: [Cup] -> Int -> ST s (V.MVector s Int)
seed initial amountNeeded =
  do
    cups <- V.new (amountNeeded + 1)
    let values = initial ++ [10, 11 ..]
    let cupAndSubsequentCupPairs = zip values $ tail values
    forM_ (take amountNeeded cupAndSubsequentCupPairs) $ uncurry (V.write cups)
    let end = if amountNeeded > length initial then amountNeeded else last initial
    V.write cups 0 (head values)
    V.write cups end (head values)
    return cups

type Cup = Int

testInput :: [Cup]
testInput = [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: [Cup]
input = [4, 6, 7, 5, 2, 8, 1, 9, 3]
