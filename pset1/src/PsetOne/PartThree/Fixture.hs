{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module PsetOne.PartThree.Fixture where

import           PsetOne.PartThree.Schema
import           PsetOne.Util

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types

import           Data.Int
import           Data.Text                      ( Text )
import           Data.Time

-- helper to print out each item on new line;
-- query is some query that returns an (IO item) type
showLines :: (Show a) => IO [a] -> IO ()
showLines = flip (>>=) (\list -> mapM_ (putStrLn . show) list)

-- helper to insert tuples into a table and return the created entries
-- (the tuples may contain incomplete information; e.g., it may not have the
-- autocomputed ID column that is generated by the DB on insert)
-- (the large context is necessary to make `runInsertReturningList` happy)
makeInserter
  :: ( Beamable t
     , Projectible Postgres (t (QExpr Postgres ()))
     , FromBackendRow Postgres (t Identity)
     )
  => DatabaseEntity Postgres CompanyDb (TableEntity t)
  -> (forall s . a -> t (QExpr Postgres s))
  -> [a]
  -> IO [t Identity]
makeInserter table entryConverter entries =
  run $ runInsertReturningList $ insert table $ insertExpressions $ map
    entryConverter
    entries

insertSailors = makeInserter sailorsTable $ \(name, rating, (yyyy, mm, dd)) ->
  Sailor default_ (val_ name) (val_ rating) (val_ (fromGregorian yyyy mm dd))

insertEmployees =
  makeInserter employeesTable
    $ (\(name, (yyyy, mm, dd), wage) -> Employee
        default_
        (val_ name)
        (val_ (fromGregorian yyyy mm dd))
        (val_ wage)
      )

insertBoats =
  makeInserter boatsTable
    $ (\(name, color, length) ->
        Boat default_ (val_ name) (val_ color) (val_ length)
      )

insertEquipment =
  makeInserter equipmentTable
    $ (\(name, dsc, count, cost) ->
        Equipment default_ (val_ name) (val_ dsc) (val_ count) (val_ cost)
      )

-- TODO: factor this with `makeInserter` if possible
insertClockTimes
  :: (forall s . [(EmployeeT (QExpr Postgres s), LocalTime, Bool)])
  -> IO [ClockTime]
insertClockTimes entries =
  run
    $ runInsertReturningList
    $ insert clockTimesTable
    $ insertExpressions
    $ map
        (\(employee, timestamp, clockType) ->
          ClockTime (pk employee) (val_ timestamp) (val_ clockType)
        )
        entries

-- TODO: reservations
-- TODO: incidents
-- TODO: payments
-- TODO: equipment sales

createFixture = do
  resetSchema
  -- names generated from http://listofrandomnames.com
  -- numbers generated from https://www.random.org/integers
  newSailors@[hershel, joenn, vania, katheryn, shanika, madeleine, li, zachariah, marinda, clara] <-
    insertSailors
      [ ("Hershel"  , 7 , (1993, 3, 23))
      , ("Joeann"   , 1 , (1964, 10, 16))
      , ("Vania"    , 5 , (1983, 5, 16))
      , ("Katheryn" , 2 , (1997, 10, 19))
      , ("Shanika"  , 10, (2001, 3, 16))
      , ("Madeleine", 6 , (1963, 9, 11))
      , ("Li"       , 6 , (1990, 9, 17))
      , ("Zachariah", 6 , (1994, 1, 9))
      , ("Marinda"  , 5 , (1983, 8, 22))
      , ("Clara"    , 3 , (1987, 3, 24))
      ]
  newEmployees@[marsha, willard, bryon, chanelle, vikki] <- insertEmployees
    [ ("Marsha"  , (1992, 10, 10), 2000)
    , ("Willard" , (1982, 3, 22) , 2000)
    , ("Bryon"   , (1956, 11, 13), 4500)
    , ("Chanelle", (1977, 11, 2) , 3000)
    , ("Vikki"   , (2002, 5, 30) , 2000)
    ]
  -- boat names from:
  -- https://www.boatus.com/products-and-services/boat-lettering/boat-names
  newBoats@[andiamo, socialDistancing, grace, shenanigans, coolChange, knotOnCall] <-
    insertBoats
      [ ("Andiamo"          , "red"  , 57)
      , ("Social Distancing", "green", 21)
      , ("Grace"            , "blue" , 52)
      , ("Shenanigans"      , "green", 49)
      , ("Cool Change"      , "red"  , 25)
      , ("Knot On Call"     , "red"  , 15)
      ]
  -- sample equipment from:
  -- https://www.velasailingsupply.com/sailboat-equipment/sailing-accessories/
  newEquipment@[boatHookEnd, reelTreatment, eyestrapKit, marineRotationPlate] <-
    insertEquipment
      [ ("Boat Hook End", "Allen Brothers 25MM Nylon Boat Hook End", 42, 723)
      , ("Reel Treatment", "ReelX Performance Reel Treatment", 10, 620)
      , ( "Soft eyestrap kit"
        , "This kit contains webbing and rivet with washer to mount this soft eyestrap, class rules let you replace the stock stainless steel lacing eyestrap in the middle of your boom with this soft strap."
        , 5
        , 707
        )
      , ( "Johnson Marine Rotation Plate"
        , "This plate allows 45deg of rotation in either vertical or horizontal plane. The plate mounts between the bracket and accessory unit on any of our \"40\" series mounts."
        , 15
        , 743
        )
      ]
  pure newSailors
