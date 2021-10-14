{-# LANGUAGE OverloadedStrings #-}

module PsetOne.PartThree.QueriesSpec
  ( spec
  ) where

import           PsetOne.PartThree.Fixture
import           PsetOne.PartThree.Queries
import           PsetOne.PartThree.Schema

import           Data.HashSet

import           Test.Hspec

spec :: Spec
spec = before_ createFixture $ describe "part 3 test cases" $ do
  it "get sailors who have reserved all red boats" $ do
    result <- querySailorsReservedAllRedBoats
    shouldBe (fromList result)
      $ fromList [(SailorId 3, "Vania"), (SailorId 1, "Hershel")]

  it "get sailors who have reserved only red boats" $ do
    result <- querySailorsReservedOnlyRedBoats
    shouldBe (fromList result) $ fromList [(SailorId 3, "Vania")]

  it "get sailors along with how much they have spent" $ do
    result <- querySailorsSpending
    shouldBe (fromList result) $ fromList
      [ (SailorId 1 , "Hershel"  , 13531.12)
      , (SailorId 9 , "Marinda"  , 7.07)
      , (SailorId 5 , "Shanika"  , 0.5)
      , (SailorId 4 , "Katheryn" , 0.5)
      , (SailorId 8 , "Zachariah", 0.5)
      , (SailorId 2 , "Joeann"   , 1.5)
      , (SailorId 10, "Clara"    , 7.23)
      , (SailorId 7 , "Li"       , 0.5)
      , (SailorId 6 , "Madeleine", 14.46)
      , (SailorId 3 , "Vania"    , 51.5)
      ]

  it
      (  "get all pairs of sailors and employees who have "
      ++ "met through some transaction (i.e., through "
      ++ "reservations or incidents)"
      )
    $ do
        result <- querySailorsServicedEmployees
        shouldBe (fromList result) $ fromList
          [ (EmployeeId 4, "Chanelle", SailorId 2, "Joeann")
          , (EmployeeId 1, "Marsha"  , SailorId 1, "Hershel")
          , (EmployeeId 4, "Chanelle", SailorId 8, "Zachariah")
          , (EmployeeId 2, "Willard" , SailorId 8, "Zachariah")
          , (EmployeeId 2, "Willard" , SailorId 7, "Li")
          , (EmployeeId 5, "Vikki"   , SailorId 1, "Hershel")
          , (EmployeeId 5, "Vikki"   , SailorId 2, "Joeann")
          , (EmployeeId 3, "Bryon"   , SailorId 2, "Joeann")
          , (EmployeeId 4, "Chanelle", SailorId 3, "Vania")
          , (EmployeeId 5, "Vikki"   , SailorId 5, "Shanika")
          , (EmployeeId 4, "Chanelle", SailorId 1, "Hershel")
          , (EmployeeId 5, "Vikki"   , SailorId 4, "Katheryn")
          ]

  it "get the sailor who has bought the most boat hook ends" $ do
    result <- querySailorMostBoatHookEnds
    shouldBe (fromList result) $ fromList [(SailorId 1, "Hershel", 3)]

  it "count the total number of hours employees have worked" $ do
    result <- queryTotalHoursWorked
    shouldSatisfy result (\x -> abs (x - 120.1) < 0.05)
