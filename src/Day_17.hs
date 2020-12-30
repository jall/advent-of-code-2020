module Day_17
  ( solution,
  )
where

import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V3 (..), V4 (..))
import Utils (mapWithIndex)

solution :: (Maybe String, Maybe String)
solution =
  let grid = toGridV3 input
      cycles = 6
   in ( Just . show $
          let rounds = take (cycles + 1) $ iterate update grid
           in S.size $ last rounds,
        Nothing
      )

update :: (Coord a) => Grid a -> Grid a
update grid =
  let stillAlive = cubesSurviving grid
      freshlyBorn = cubesBorn grid
   in S.union stillAlive freshlyBorn

cubesSurviving :: (Coord a) => Grid a -> Set a
cubesSurviving grid =
  S.filter
    ( \cube -> case countLiveNeighbours grid cube of
        2 -> True
        3 -> True
        _ -> False
    )
    grid

cubesBorn :: (Coord a) => Grid a -> Set a
cubesBorn grid =
  S.filter
    ( \cube -> case countLiveNeighbours grid cube of
        3 -> True
        _ -> False
    )
    $ deadGridNeighbours grid

deadGridNeighbours :: (Coord a) => Grid a -> Set a
deadGridNeighbours grid = (`S.difference` grid) $ S.unions $ S.map neighbours grid

toGridV3 :: [[Char]] -> Grid (V3 Int)
toGridV3 cubes =
  S.fromList $
    map (\(_, (x, y)) -> V3 x y 0) $
      filter fst $
        concat $
          mapWithIndex (\row y -> mapWithIndex (\cube x -> (cube == active, (x, y))) row) cubes

countLiveNeighbours :: (Coord a) => Grid a -> a -> Int
countLiveNeighbours grid coord = S.size $ S.intersection grid $ neighbours coord

active = '#'

type Grid a = Set a

class (Num a, Ord a) => Coord a where
  neighbours :: a -> Set a

instance Coord (V3 Int) where
  neighbours (V3 x y z) =
    S.fromList
      [ V3 x' y' z'
        | x' <- [x - 1, x, x + 1],
          y' <- [y - 1, y, y + 1],
          z' <- [z - 1, z, z + 1],
          (x', y', z') /= (x, y, z)
      ]

testInput :: [[Char]]
testInput =
  [ ".#.",
    "..#",
    "###"
  ]

input :: [[Char]]
input =
  [ "#.#..#.#",
    "#.......",
    "####..#.",
    ".#.#.##.",
    "..#..#..",
    "###..##.",
    ".#..##.#",
    ".....#.."
  ]
