{-# LANGUAGE OverloadedStrings #-}

module QueriesSpec
  ( spec
  ) where

import           Queries                        ( pset1Query1 )
import           Test.Hspec

spec :: Spec
spec = describe "Checking correct output for queries" $ do
  it
      (  "pset 1 query 1: list, for every boat, the number of "
      ++ "times it has been reserved, excluding those boats "
      ++ "that have never been reserved"
      )
    $ do
        result <- pset1Query1
        shouldBe
          result
          [ (101, "INTerlake", 2)
          , (102, "INTerlake", 3)
          , (103, "Clipper"  , 3)
          , (104, "Clipper"  , 5)
          , (105, "Marine"   , 3)
          , (106, "Marine"   , 3)
          , (107, "Marine"   , 1)
          , (108, "Driftwood", 1)
          , (109, "Driftwood", 4)
          , (110, "Klapser"  , 3)
          , (111, "Sooney"   , 1)
          , (112, "Sooney"   , 1)
          ]
