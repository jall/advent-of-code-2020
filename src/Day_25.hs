module Day_25 (solution) where

import Data.Bifunctor (bimap, first)
import Data.Function.Memoize (memoize)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

solution :: (Maybe String, Maybe String)
solution =
  let (cardPublicKey, doorPublicKey) = input
      doorLoopSize = findLoopSize doorPublicKey subjectNumber
   in ( Just . show $ iterate (transform cardPublicKey) initialValue !! doorLoopSize,
        Just . show $ doorLoopSize
      )

findLoopSize :: PublicKey -> Int -> Int
findLoopSize publicKey subjectNumber =
  snd $
    until
      ((== publicKey) . fst)
      (bimap (transform subjectNumber) (+ 1))
      (initialValue, 0)

transform :: Int -> Int -> Int
transform subject value = (subject * value) `mod` 20201227

initialValue = 1

subjectNumber = 7

type PublicKey = Int

testInput :: (PublicKey, PublicKey)
testInput = (5764801, 17807724)

input :: (PublicKey, PublicKey)
input = (335121, 363891)
