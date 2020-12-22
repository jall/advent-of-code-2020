module Utils
  ( mapWithIndex,
  )
where

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0 ..]
