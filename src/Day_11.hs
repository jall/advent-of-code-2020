module Day_11
  ( solution,
  )
where

import Control.Lens (element, (^?))
import Data.Maybe (mapMaybe)
import Utils (countIf, mapWithIndex)

solution :: (Maybe String, Maybe String)
solution =
  let grid = input
   in ( Just . show $
          let stableGrid = updateGridUntilStable grid
           in countOccupied stableGrid,
        Nothing
      )

countOccupied :: Grid -> Int
countOccupied = foldr (\row total -> total + countIf (== occupied) row) 0

updateGridUntilStable :: Grid -> Grid
updateGridUntilStable grid =
  let nextGrid = updateGrid grid
   in if grid == nextGrid
        then grid
        else updateGridUntilStable nextGrid

updateGrid :: Grid -> Grid
updateGrid grid =
  mapWithIndex
    (\row y -> mapWithIndex (\seat x -> updateSeat grid seat (x, y)) row)
    grid

updateSeat :: Grid -> Char -> (Int, Int) -> Char
updateSeat grid seat (x, y) =
  let numberOfOccupiedSeats = length $ filter (== occupied) $ adjacentSeats grid (x, y)
   in case () of
        _
          | seat == empty && numberOfOccupiedSeats == 0 -> occupied
          | seat == occupied && numberOfOccupiedSeats >= 4 -> empty
          | otherwise -> seat

adjacentSeats :: Grid -> (Int, Int) -> [Char]
adjacentSeats grid (x, y) =
  mapMaybe
    (findSeat grid)
    [ (x - 1, y - 1),
      (x - 1, y),
      (x - 1, y + 1),
      (x, y - 1),
      (x, y + 1),
      (x + 1, y - 1),
      (x + 1, y),
      (x + 1, y + 1)
    ]

findSeat :: Grid -> (Int, Int) -> Maybe Char
findSeat grid (x, y) =
  do
    row <- grid ^? element y
    row ^? element x

floor = '.'

occupied = '#'

empty = 'L'

type Grid = [[Char]]

testInput =
  [ "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL"
  ]

input =
  [ "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL..LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LL.LLLLLLL",
    "LLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL",
    "LL.LLLLLLLLLLLLLLLLLLLL..LLLLLL..LLLLLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLL",
    "LLLL.LLLLL.LLL.LLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLL.LL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLL..LLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLL..LLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL..LLLLLLLLLL.LLLL",
    "LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL..LLLL.LLLLLLLLLLLLLLL.LLL.LLLLLLLLLLLLL",
    "LLLLLLLLLL.LL.L.LLLLLLL.LLLLLL..LLLLLLLL.LLLLLLL.L.LLLL.LLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL",
    "LLLL.LLLLL.LLLL.L.LLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLL.LLLL..LLLLLLLLL",
    "L..LL.L.LL.........LLL...LLL.........LL..LL.L..LL..L...L..L....LL.LL.L.LL.L..L...LLL......LL.L",
    "LL.LLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLL.L.LLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLL",
    "LLLLLLLLLL.LLLL.L.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLL.LL.LLLLLLLLLLLL.L.LLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLL.LL.LLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL",
    "...L..L.LL......L..L..LL..LLL..L.L.L....L.LLLLL...L...L....L..L..........L..LL.LL.L........LL.",
    "LLLLLLLLLLLLLLL.LLLLL.L.LLLLLLL.LLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLL.LLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLL..LLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLL",
    ".LLLLLLLLL.LLLL..LLLLLL.LLLLLL..LLLLL.LL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLL.LLLL.LLLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LL.L.LLLLLLL..LLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.L.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LL.LLLLLLLLLLLLLLL",
    "LLLLLLLL.L.LLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLL..LLLLL..LLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL",
    ".LL..LLL.LLL..LL.LL....LLLL.....LLLL..LLLL.L.L...........L...L.LLLLLL..LL..LL.LL.L.....LL.L...",
    "LLLLLLLL.LLLLLL.LLLLLLLLLL.L.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL..LLLLLL.LLLLLLLLLLL.LL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLL.LLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL..LLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLL..LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLL.LLL.LLLL.LLLLLLL.LL",
    "...............L.....L.....L....L.L....L.L.L.L.L.L..L..LL..L.......L.LLL.L.L.L...L..L.L.L..LLL",
    "LLLLLLLL.LLLLLL.LLLLLLL.LLLLLLL..LLLLLLLLLLLLLLL..LLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL..LLLLLLLLL",
    "L.LLLLLLLL.LLLLLLLLLLLL.LLLLL.L.LL.LLLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLL.LLL.LLLL.LLLLLLLL.L",
    "LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL..LLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLL..LLLLLL.LLLLLLL.LLLLLLLLL.LL.LLL.LLLLLLLLLLL.LLLLLL..LLLLLL.LL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLL.L.LLLLL.LLLLLLL.LLLLLLLL.L.LLLLL.LLLLLL.LLLL.LLLLL.LLLLLLLLLLL.L..L.LLLLLLLLLL",
    "LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLL.L.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLL.L.LLLLLLLL",
    ".LLL..L.L.LL.L.LL.LL.LLL.L..L.L.....L....L...L.L......L.LL....L.LL..L.......L..L...L..L...LLL.",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLL..LLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLLLLLL.L.LLLLLLL..LLLL.LLLLLLLLL",
    "L..LLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLLL.LL.LLLLLLLLLLLLLLLLLLL.LLL.LLL.LL.LLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL..LLLLLLLLLLLLLLL.LLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLL.LL",
    "LLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLLLL..LLLLLLLLLLLLLLLLLL..LLLLLLL.LLLLLLLLL.LL.LLLLLL.LLLLL",
    "LLL.LLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL",
    "..L...LLL......L....L..LL..LLLLL..L...L..LL.L.L....L...L.LL..L..L.LLL.....L.L...L......L.LL..L",
    "LL.LLLLLLL.LLLL.LLLL.LL.LLLLLLL..LLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLL...LLL.L.LLLLLLLL",
    "LLLLLLLLLL.L.LL.L.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLLL.L.LLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.LL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLL.LL.LLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLL",
    "L.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLL.LLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLL",
    "LL......LL...L......L.L...LL..L.L...LL.L........L.LL.LL.LL...L...LL.....L..LL.L....L.L..LLL..L",
    "LLLLLLLLLL.LLLL.L.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLL.LLLLLL.LL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLL",
    "LLLL.LL.LL.LLLL.LLLLLLLLLL..LLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.L.LLLLLLLL",
    "LLLL.LLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLL..LLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLL.LLL.LLLLLLL.LLLLLLLLL.",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.L..LLLLLLLLLLL.LLL.LLLLLL",
    "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLL.LL.LLLL.LLLLLL..LLLLLLLLL.LLLL.LLLLLLLLLL",
    ".....LL..LL.......L.L.....L.....L....L..L.L...L.L.L.L...L.L..L.LLL.............L...........L.L",
    "L.LLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL.LLLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLL..LLLLLLLLLLLLLLLLLLLLL.LL.L.L.LLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLLLL.LLLLLL..LLLLLLLLLL",
    "LLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL",
    "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL..LLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLL.LLL.LLLLLLL.LLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL",
    ".LLL.....L...L.LLL.L.....L.L........L..LL.L.L.LL.....L..L.L.LL.LLLLLL..LL...L..L...LL.L.LL....",
    "LLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLL.LLLLL.L.LLLLLLLLL.LLLL.LLLLLLL.LL",
    "LLLLLLLL.LLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLL.LL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LL.LLLL.LLLLLLLL.LLLLLLL.LLLLLL.LLLL...LLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLL.LLL",
    "LLLLLLLLL..LLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL",
    ".L..L.L.LL..........L..L.............LLLL..L.......LL....L..L.......L.........L..L.L..LL....LL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLL.LL",
    "LLLLLLLLLL.LLLL.LLL.LLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL",
    "LLLLL.LLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLL.L.LLLLLLL.LLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL",
    "..L.....L......L....L..LLL..L.L..L....L..LL....L.L........LL...L.......L..L.L.L..LL..L.L.....L",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LL.LLLLLLLLLLLLL.LLLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLL..LLLLLLLLLL",
    "LLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LL..LLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLL..LL.L.LLLLLLL.LL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLLLLL..LLL.LLLLLLLLLLLLLL.LLL.LLL.LLL.LLLLLLLLLLLLLLLLLLLL.",
    "LLLLLLLLLL.LL.L.LLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL",
    "LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLLL.LLLL.LLLLLLL.LLLLLLLLLLL.LL.LLLLLLLLLL"
  ]
