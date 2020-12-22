import qualified Day_01
import qualified Day_02
import qualified Day_03
import qualified Day_04
import qualified Day_05
import qualified Day_06
import qualified Day_07
import qualified Day_08
import qualified Day_09
import qualified Day_10
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day1, day2, day3, day4, day5, day6, day7, day8, day9, day10]

day1 = testCase "Day 1" $ assertEqual "" (Just "252724", Just "276912720") Day_01.solution

day2 = testCase "Day 2" $ assertEqual "" (Just "600", Just "245") Day_02.solution

day3 = testCase "Day 3" $ assertEqual "" (Just "270", Just "2122848000") Day_03.solution

day4 = testCase "Day 4" $ assertEqual "" (Just "192", Just "101") Day_04.solution

day5 = testCase "Day 5" $ assertEqual "" (Just "913", Just "717") Day_05.solution

day6 = testCase "Day 6" $ assertEqual "" (Just "6443", Just "3232") Day_06.solution

day7 = testCase "Day 7" $ assertEqual "" (Just "278", Just "45157") Day_07.solution

day8 = testCase "Day 8" $ assertEqual "" (Just "1179", Just "1089") Day_08.solution

day9 = testCase "Day 9" $ assertEqual "" (Just "133015568", Just "16107959") Day_09.solution

day10 = testCase "Day 10" $ assertEqual "" (Just "1690", Nothing) Day_10.solution
