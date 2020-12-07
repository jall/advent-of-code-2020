import qualified Day_01
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day1]

day1 = testCase "Day 1:1" $ assertEqual "" Day_01.solution (Just "252724")
