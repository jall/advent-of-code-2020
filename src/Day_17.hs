module Day_17
  ( solution,
  )
where

import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V3
import Utils (mapWithIndex)

solution :: (Maybe String, Maybe String)
solution =
  let grid = toGrid input
      cycles = 6
   in ( Just . show $
          let rounds = take (cycles + 1) $ iterate update grid
           in S.size $ last rounds,
        Nothing
      )

update :: Grid -> Grid
update grid =
  let stillAlive = cubesSurviving grid
      freshlyBorn = cubesBorn grid
   in S.union stillAlive freshlyBorn

cubesSurviving :: Grid -> Set Coord
cubesSurviving grid =
  S.filter
    ( \cube -> case countLiveNeighbours grid cube of
        2 -> True
        3 -> True
        _ -> False
    )
    grid

cubesBorn :: Grid -> Set Coord
cubesBorn grid =
  S.filter
    ( \cube -> case countLiveNeighbours grid cube of
        3 -> True
        _ -> False
    )
    $ deadGridNeighbours grid

deadGridNeighbours :: Grid -> Set Coord
deadGridNeighbours grid = (`S.difference` grid) $ S.unions $ S.map neighbours grid

toGrid :: [[Char]] -> Grid
toGrid cubes =
  S.fromList $
    map (\(_, (x, y)) -> V3 x y 0) $
      filter fst $
        concat $
          mapWithIndex (\row y -> mapWithIndex (\cube x -> (cube == active, (x, y))) row) cubes

countLiveNeighbours :: Grid -> Coord -> Int
countLiveNeighbours grid coord = S.size $ S.intersection grid $ neighbours coord

neighbours :: Coord -> Set Coord
neighbours (V3 x y z) =
  S.fromList
    [ V3 x' y' z'
      | x' <- [x - 1, x, x + 1],
        y' <- [y - 1, y, y + 1],
        z' <- [z - 1, z, z + 1],
        (x', y', z') /= (x, y, z)
    ]

active = '#'

type Grid = Set Coord

type Coord = V3 Int

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
