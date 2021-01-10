module Day_22 (solution) where

solution :: (Maybe String, Maybe String)
solution =
  let startingDecks = input
      rounds = iterate playRound startingDecks
   in ( Just . show $
          let (one, two) = head $ dropWhile gameUnfinished rounds
           in if null one then score two else score one,
        Nothing
      )

score :: Deck -> Int
score deck = sum $ zipWith (*) (reverse deck) [1 ..]

gameUnfinished :: (Deck, Deck) -> Bool
gameUnfinished (one, two) = (not . null) one && (not . null) two

playRound :: (Deck, Deck) -> (Deck, Deck)
playRound ([], two) = ([], two)
playRound (one, []) = (one, [])
playRound (one : ones, two : twos) =
  if one > two
    then (ones ++ [one, two], twos)
    else (ones, twos ++ [two, one])

type Deck = [Int]

testInput :: (Deck, Deck)
testInput =
  ( [ 9,
      2,
      6,
      3,
      1
    ],
    [ 5,
      8,
      4,
      7,
      10
    ]
  )

input :: (Deck, Deck)
input =
  ( [ 38,
      39,
      42,
      17,
      13,
      37,
      4,
      10,
      2,
      34,
      43,
      41,
      22,
      24,
      46,
      19,
      30,
      50,
      6,
      44,
      28,
      27,
      36,
      5,
      45
    ],
    [ 31,
      40,
      25,
      11,
      3,
      48,
      16,
      9,
      33,
      7,
      12,
      35,
      49,
      32,
      26,
      47,
      14,
      8,
      20,
      23,
      1,
      29,
      15,
      21,
      18
    ]
  )
