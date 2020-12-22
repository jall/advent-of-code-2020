module Utils
  ( mapWithIndex,
    countIf,
  )
where

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0 ..]

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p
