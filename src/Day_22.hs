module Day_22 (solution) where

solution :: (Maybe String, Maybe String)
solution =
  let startingRound = input
   in ( Just . show $
          let rounds = iterate playSimple startingRound
              finalRound = head $ dropWhile gameUnfinished rounds
           in score $ winner finalRound,
        Just . show $
          let finalRound = playRecursive [] startingRound
           in score $ winner finalRound
      )

playRecursive :: [Round] -> Round -> Round
playRecursive previousRounds (ones, []) = (ones, [])
playRecursive previousRounds ([], twos) = ([], twos)
playRecursive previousRounds currentRound
  | currentRound `elem` previousRounds = (fst currentRound, [])
  | otherwise =
    let (one : ones, two : twos) = currentRound
        didOneWin =
          if length ones >= one && length twos >= two
            then
              let (subOne, subTwo) = playRecursive [] (take one ones, take two twos)
               in null subTwo
            else one > two
     in playRecursive (currentRound : previousRounds) (addCardsToWinner didOneWin currentRound)

winner :: Round -> Deck
winner (one, []) = one
winner ([], two) = two

score :: Deck -> Int
score deck = sum $ zipWith (*) (reverse deck) [1 ..]

gameUnfinished :: Round -> Bool
gameUnfinished (one, two) = (not . null) one && (not . null) two

playSimple :: Round -> Round
playSimple ([], two) = ([], two)
playSimple (one, []) = (one, [])
playSimple round =
  let (one : _, two : _) = round in addCardsToWinner (one > two) round

addCardsToWinner :: Bool -> Round -> Round
addCardsToWinner didOneWin (one : ones, two : twos) =
  if didOneWin
    then (ones ++ [one, two], twos)
    else (ones, twos ++ [two, one])

type Round = (Deck, Deck)

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

infiniteLoopInput :: (Deck, Deck)
infiniteLoopInput =
  ( [43, 19],
    [ 2,
      29,
      14
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
