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
  let cubes = input
      cycles = 6
   in ( Just . show $
          let rounds = take (cycles + 1) $ iterate update $ to3DGrid cubes
           in S.size $ last rounds,
        Just . show $
          let rounds = take (cycles + 1) $ iterate update $ to4DGrid cubes
           in S.size $ last rounds
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

to3DGrid :: [[Char]] -> Grid (V3 Int)
to3DGrid cubes =
  S.fromList $
    map (\(_, (x, y)) -> V3 x y 0) $
      filter fst $
        concat $
          mapWithIndex (\row y -> mapWithIndex (\cube x -> (cube == active, (x, y))) row) cubes

to4DGrid :: [[Char]] -> Grid (V4 Int)
to4DGrid cubes = S.map (\(V3 x y z) -> V4 x y z 0) $ to3DGrid cubes

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

instance Coord (V4 Int) where
  neighbours (V4 x y z a) =
    S.fromList
      [ V4 x' y' z' a'
        | x' <- [x - 1, x, x + 1],
          y' <- [y - 1, y, y + 1],
          z' <- [z - 1, z, z + 1],
          a' <- [a - 1, a, a + 1],
          (x', y', z', a') /= (x, y, z, a)
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
