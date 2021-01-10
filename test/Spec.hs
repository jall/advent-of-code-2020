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
import qualified Day_11
import qualified Day_12
import qualified Day_13
import qualified Day_14
import qualified Day_15
import qualified Day_16
import qualified Day_17
import qualified Day_18
import qualified Day_19
import qualified Day_20
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25]

day1 = testCase "Day 1" $ assertEqual "" (Just "252724", Just "276912720") Day_01.solution

day2 = testCase "Day 2" $ assertEqual "" (Just "600", Just "245") Day_02.solution

day3 = testCase "Day 3" $ assertEqual "" (Just "270", Just "2122848000") Day_03.solution

day4 = testCase "Day 4" $ assertEqual "" (Just "192", Just "101") Day_04.solution

day5 = testCase "Day 5" $ assertEqual "" (Just "913", Just "717") Day_05.solution

day6 = testCase "Day 6" $ assertEqual "" (Just "6443", Just "3232") Day_06.solution

day7 = testCase "Day 7" $ assertEqual "" (Just "278", Just "45157") Day_07.solution

day8 = testCase "Day 8" $ assertEqual "" (Just "1179", Just "1089") Day_08.solution

day9 = testCase "Day 9" $ assertEqual "" (Just "133015568", Just "16107959") Day_09.solution

day10 = testCase "Day 10" $ assertEqual "" (Just "1690", Just "5289227976704") Day_10.solution

day11 = testCase "Day 11" $ assertEqual "" (Just "2251", Just "2019") Day_11.solution

day12 = testCase "Day 12" $ assertEqual "" (Just "2057", Just "71504") Day_12.solution

day13 = testCase "Day 13" $ assertEqual "" (Just "207", Just "530015546283687") Day_13.solution

day14 = testCase "Day 14" $ assertEqual "" (Just "10452688630537", Just "2881082759597") Day_14.solution

day15 = testCase "Day 15" $ assertEqual "" (Just "1373", Just "112458") Day_15.solution

day16 = testCase "Day 16" $ assertEqual "" (Just "24110", Just "6766503490793") Day_16.solution

day17 = testCase "Day 17" $ assertEqual "" (Just "324", Just "1836") Day_17.solution

day18 = testCase "Day 18" $ assertEqual "" (Just "464478013511", Just "85660197232452") Day_18.solution

day19 = testCase "Day 19" $ assertEqual "" (Just "156", Just "363") Day_19.solution

day20 = testCase "Day 20" $ assertEqual "" (Just "59187348943703", Nothing) Day_20.solution

day21 = testCase "Day 21" $ assertEqual "" (Just "2734", Just "kbmlt,mrccxm,lpzgzmk,ppj,stj,jvgnc,gxnr,plrlg") Day_21.solution

day22 = testCase "Day 22" $ assertEqual "" (Just "31308", Just "33647") Day_22.solution

day23 = testCase "Day 23" $ assertEqual "" (Nothing, Nothing) Day_23.solution

day24 = testCase "Day 24" $ assertEqual "" (Nothing, Nothing) Day_24.solution

day25 = testCase "Day 25" $ assertEqual "" (Nothing, Nothing) Day_25.solution
