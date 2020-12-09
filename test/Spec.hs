import qualified Day_01
import qualified Day_02
import qualified Day_03
import qualified Day_04
import qualified Day_05
import qualified Day_06
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day1, day2, day3, day4, day5, day6]

day1 = testCase "Day 1" $ assertEqual "" Day_01.solution (Just "252724", Just "276912720")

day2 = testCase "Day 2" $ assertEqual "" Day_02.solution (Just "600", Just "245")

day3 = testCase "Day 3" $ assertEqual "" Day_03.solution (Just "270", Just "2122848000")

day4 = testCase "Day 4" $ assertEqual "" Day_04.solution (Just "192", Just "101")

day5 = testCase "Day 5" $ assertEqual "" Day_05.solution (Just "913", Just "717")

day6 = testCase "Day 6" $ assertEqual "" Day_06.solution (Just "6443", Just "3232")
