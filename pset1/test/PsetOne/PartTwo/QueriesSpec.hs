{-# LANGUAGE OverloadedStrings #-}

module PsetOne.PartTwo.QueriesSpec
  ( spec
  ) where

import           PsetOne.PartTwo.Queries
import           PsetOne.PartTwo.Schema

import           Data.HashSet

import           Test.Hspec

-- this assumes the test fixture from ../../pset1_setup.sql
-- check for unordered (set) equality, since order isn't specified

spec :: Spec
spec = describe "part 2 test cases" $ do
  it
      (  "query 1: list, for every boat, the number of "
      ++ "times it has been reserved, excluding those boats "
      ++ "that have never been reserved"
      )
    $ do
        result <- pset1Query1
        shouldBe (fromList result) $ fromList
          -- bid, bname, # reservations
          [ (BoatId 101, "Interlake", 2)
          , (BoatId 102, "Interlake", 3)
          , (BoatId 103, "Clipper"  , 3)
          , (BoatId 104, "Clipper"  , 5)
          , (BoatId 105, "Marine"   , 3)
          , (BoatId 106, "Marine"   , 3)
          , (BoatId 107, "Marine"   , 1)
          , (BoatId 108, "Driftwood", 1)
          , (BoatId 109, "Driftwood", 4)
          , (BoatId 110, "Klapser"  , 3)
          , (BoatId 111, "Sooney"   , 1)
          , (BoatId 112, "Sooney"   , 1)
          ]

  it ("query 2: list those sailors who have reserved every red boat") $ do
    result <- pset1Query2
    -- sid, sname
    shouldBe (fromList result) $ fromList []

  it ("query 3: list those sailors who have reserved only red boats") $ do
    result <- pset1Query3
    shouldBe (fromList result) $ fromList
      -- sid, sname
      [ (SailorId 23, "emilio")
      , (SailorId 24, "scruntus")
      , (SailorId 35, "figaro")
      , (SailorId 61, "ossola")
      , (SailorId 62, "shaun")
      ]

  it ("query 4: for which boat are there the most reservations") $ do
    result <- pset1Query4
    -- bid, bname, number of reservations
    shouldBe (fromList result) $ fromList [(BoatId 104, "Clipper", 5)]

  it ("query 5: select all sailors who have never reserved a red boat") $ do
    result <- pset1Query5
    shouldBe (fromList result) $ fromList
      -- sid, sname
      [ (SailorId 29, "brutus")
      , (SailorId 32, "andy")
      , (SailorId 58, "rusty")
      , (SailorId 60, "jit")
      , (SailorId 71, "zorba")
      , (SailorId 74, "horatio")
      , (SailorId 85, "art")
      , (SailorId 90, "vin")
      , (SailorId 95, "bob")
      ]

  it ("query 6: average age of sailors with a rating of 10") $ do
    result <- pset1Query6
    shouldBe result 35.0

  it
      (  "query 7: for each rating, find the name and id "
      ++ "of the youngest sailor"
      )
    $ do
        result <- pset1Query7
        shouldBe (fromList result) $ fromList
          -- sid, sname, rating, age
          [ (SailorId 24, "scruntus", 1 , 33)
          , (SailorId 29, "brutus"  , 1 , 33)
          , (SailorId 89, "dye"     , 3 , 25)
          , (SailorId 85, "art"     , 3 , 25)
          , (SailorId 61, "ossola"  , 7 , 16)
          , (SailorId 64, "horatio" , 7 , 16)
          , (SailorId 59, "stum"    , 8 , 25)
          , (SailorId 32, "andy"    , 8 , 25)
          , (SailorId 74, "horatio" , 9 , 25)
          , (SailorId 88, "dan"     , 9 , 25)
          , (SailorId 58, "rusty"   , 10, 35)
          , (SailorId 62, "shaun"   , 10, 35)
          , (SailorId 60, "jit"     , 10, 35)
          , (SailorId 71, "zorba"   , 10, 35)
          ]

  it
      (  "query 8: select, for each boat, the sailor who made "
      ++ "the highest number of reservations for that boat"
      )
    $ do
        result <- pset1Query8
        shouldBe (fromList result) $ fromList
          -- bid, sid, sname, reservation count
          [ (BoatId 101, SailorId 22, "dusting" , 1)
          , (BoatId 101, SailorId 64, "horatio" , 1)
          , (BoatId 102, SailorId 22, "dusting" , 1)
          , (BoatId 102, SailorId 31, "lubber"  , 1)
          , (BoatId 102, SailorId 64, "horatio" , 1)
          , (BoatId 103, SailorId 22, "dusting" , 1)
          , (BoatId 103, SailorId 31, "lubber"  , 1)
          , (BoatId 103, SailorId 74, "horatio" , 1)
          , (BoatId 104, SailorId 22, "dusting" , 1)
          , (BoatId 104, SailorId 23, "emilio"  , 1)
          , (BoatId 104, SailorId 24, "scruntus", 1)
          , (BoatId 104, SailorId 31, "lubber"  , 1)
          , (BoatId 104, SailorId 35, "figaro"  , 1)
          , (BoatId 105, SailorId 23, "emilio"  , 1)
          , (BoatId 105, SailorId 35, "figaro"  , 1)
          , (BoatId 105, SailorId 59, "stum"    , 1)
          , (BoatId 106, SailorId 60, "jit"     , 2)
          , (BoatId 107, SailorId 88, "dan"     , 1)
          , (BoatId 108, SailorId 89, "dye"     , 1)
          , (BoatId 109, SailorId 59, "stum"    , 1)
          , (BoatId 109, SailorId 60, "jit"     , 1)
          , (BoatId 109, SailorId 89, "dye"     , 1)
          , (BoatId 109, SailorId 90, "vin"     , 1)
          , (BoatId 110, SailorId 88, "dan"     , 2)
          , (BoatId 111, SailorId 88, "dan"     , 1)
          , (BoatId 112, SailorId 61, "ossola"  , 1)
          ]
