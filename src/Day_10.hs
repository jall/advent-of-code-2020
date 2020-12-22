module Day_10
  ( solution,
  )
where

import Data.List (sort)

solution :: (Maybe String, Maybe String)
solution =
  let adapterJoltages = sort testInput
      deviceJoltage = last adapterJoltages + 3
      joltages = 0 : adapterJoltages ++ [deviceJoltage]
      differences = [b - a | (a, b) <- zipAdjacent joltages]
      oneJoltDifferences = countIf (== 1) differences
      threeJoltDifferences = countIf (== 3) differences
   in ( Just . show $ oneJoltDifferences * threeJoltDifferences,
        Nothing
      )

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

zipAdjacent :: [b] -> [(b, b)]
zipAdjacent x = zip x $ tail x

testInput =
  [ 16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  ]

testInput2 =
  [ 28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  ]

input =
  [ 114,
    51,
    122,
    26,
    121,
    90,
    20,
    113,
    8,
    138,
    57,
    44,
    135,
    76,
    134,
    15,
    21,
    119,
    52,
    118,
    107,
    99,
    73,
    72,
    106,
    41,
    129,
    83,
    19,
    66,
    132,
    56,
    32,
    79,
    27,
    115,
    112,
    58,
    102,
    64,
    50,
    2,
    39,
    3,
    77,
    85,
    103,
    140,
    28,
    133,
    78,
    34,
    13,
    61,
    25,
    35,
    89,
    40,
    7,
    24,
    33,
    96,
    108,
    71,
    11,
    128,
    92,
    111,
    55,
    80,
    91,
    31,
    70,
    101,
    14,
    18,
    12,
    4,
    84,
    125,
    120,
    100,
    65,
    86,
    93,
    67,
    139,
    1,
    47,
    38
  ]
