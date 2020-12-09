import qualified Day_01
import qualified Day_02
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day1, day2]

day1 = testCase "Day 1" $ assertEqual "" Day_01.solution (Just "252724", Just "276912720")

day2 = testCase "Day 2" $ assertEqual "" Day_02.solution (Just "600", Just "245")
