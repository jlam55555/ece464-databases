{-# LANGUAGE OverloadedStrings #-}

module PsetOne.PartThree.Fixture where

import           PsetOne.PartThree.Inserters
import           PsetOne.PartThree.Schema       ( resetSchema )
import           PsetOne.Util

-- TODO: generate some test queries to show functionality
-- TODO: write-up about everything

createFixture = do
  resetSchema
  -- names generated from http://listofrandomnames.com
  -- numbers generated from https://www.random.org/integers
  [hershel, joenn, vania, katheryn, shanika, madeleine, li, zachariah, marinda, clara] <-
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
  [marsha, willard, bryon, chanelle, vikki] <- insertEmployees
    [ ("Marsha"  , (1992, 10, 10), 2000)
    , ("Willard" , (1982, 3, 22) , 2000)
    , ("Bryon"   , (1956, 11, 13), 4500)
    , ("Chanelle", (1977, 11, 2) , 3000)
    , ("Vikki"   , (2002, 5, 30) , 2000)
    ]
  -- boat names from:
  -- https://www.boatus.com/products-and-services/boat-lettering/boat-names
  [andiamo, socialDistancing, grace, shenanigans, coolChange, knotOnCall] <-
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
  [boatHookEnd, reelTreatment, eyestrapKit, marineRotationPlate] <-
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
  insertClockTimes
    [ clockIn marsha $ makeTime 0002021 10 03 09 00 00
    , clockOut marsha $ makeTime 002021 10 03 17 01 00
    , clockIn marsha $ makeTime 0002021 10 04 08 58 00
    , clockOut marsha $ makeTime 002021 10 04 17 00 00
    , clockIn marsha $ makeTime 0002021 10 05 09 00 00
    , clockOut marsha $ makeTime 002021 10 05 17 00 00
    , clockIn marsha $ makeTime 0002021 10 06 09 00 00
    , clockOut marsha $ makeTime 002021 10 06 17 00 00
    , clockIn willard $ makeTime 002021 10 03 08 58 00
    , clockOut willard $ makeTime 02021 10 03 17 00 00
    , clockIn willard $ makeTime 002021 10 04 08 58 00
    , clockOut willard $ makeTime 02021 10 04 17 00 00
    , clockIn bryon $ makeTime 00002021 10 05 09 00 00
    , clockOut bryon $ makeTime 0002021 10 05 17 00 00
    , clockIn bryon $ makeTime 00002021 10 06 09 00 00
    , clockOut bryon $ makeTime 0002021 10 06 17 00 00
    , clockIn chanelle $ makeTime 02021 10 03 09 00 00
    , clockOut chanelle $ makeTime 2021 10 03 05 00 00
    , clockIn chanelle $ makeTime 02021 10 04 09 00 00
    , clockOut chanelle $ makeTime 2021 10 04 05 00 00
    , clockIn chanelle $ makeTime 02021 10 05 09 00 00
    , clockOut chanelle $ makeTime 2021 10 05 05 00 00
    , clockIn chanelle $ makeTime 02021 10 06 09 00 00
    , clockOut chanelle $ makeTime 2021 10 06 05 00 00
    , clockIn chanelle $ makeTime 02021 10 07 09 00 00
    , clockOut chanelle $ makeTime 2021 10 07 05 00 00
    , clockIn vikki $ makeTime 00002021 10 03 09 00 00
    , clockOut vikki $ makeTime 0002021 10 03 05 00 00
    , clockIn vikki $ makeTime 00002021 10 07 09 00 00
    , clockOut vikki $ makeTime 0002021 10 07 05 00 00
    ]
  [rsv1, rsv2, _, _, _, rsv3, _, _, rsv4, _, _, _, _, rsv5] <-
    insertReservations
      [ (hershel, andiamo, marsha, makeTime 2021 10 03 12 00 00, 50)
      , (hershel, coolChange, marsha, makeTime 2021 10 04 12 00 00, 50)
      , (hershel, shenanigans, marsha, makeTime 2021 10 05 12 00 00, 50)
      , (hershel, knotOnCall, vikki, makeTime 2021 10 07 12 00 00, 50)
      , (joenn, grace, chanelle, makeTime 2021 10 03 11 00 00, 50)
      , (joenn, grace, vikki, makeTime 2021 10 04 11 00 00, 50)
      , (joenn, knotOnCall, bryon, makeTime 2021 10 05 11 00 00, 50)
      , (vania, knotOnCall, chanelle, makeTime 2021 10 03 11 00 00, 50)
      , (vania, coolChange, chanelle, makeTime 2021 10 05 11 00 00, 50)
      , (vania, andiamo, chanelle, makeTime 2021 10 07 11 00 00, 50)
      , (katheryn, socialDistancing, vikki, makeTime 2021 10 07 11 00 00, 50)
      , (shanika, socialDistancing, vikki, makeTime 2021 10 06 11 00 00, 50)
      , (li, socialDistancing, willard, makeTime 2021 10 04 11 00 00, 50)
      , (zachariah, shenanigans, willard, makeTime 2021 10 03 11 00 00, 50)
      ]
  insertEquipmentSales
    [ (hershel  , marineRotationPlate, 1, makeTime 2021 10 03 11 50 00)
    , (hershel  , boatHookEnd        , 3, makeTime 2021 10 03 11 50 00)
    , (marinda  , eyestrapKit        , 1, makeTime 2021 10 05 11 50 00)
    , (madeleine, boatHookEnd        , 2, makeTime 2021 10 04 11 50 00)
    , (clara    , boatHookEnd        , 1, makeTime 2021 10 03 11 50 00)
    ]
  insertIncidents
    [ ( rsv1
      , makeTime 2021 10 03 13 00 02
      , 7
      , "boat ran aground"
      , marsha
      , True
      , "fixed hull"
      , Just 100000
      )
    , ( rsv1
      , makeTime 2021 10 03 14 44 02
      , 10
      , "Boat sunk"
      , chanelle
      , False
      , "bought new boat"
      , Just 1250000
      )
    , ( rsv2
      , makeTime 2021 10 04 14 44 02
      , 2
      , "Caught a big fish"
      , vikki
      , True
      , "N/A"
      , Nothing
      )
    , ( rsv3
      , makeTime 2021 10 04 14 44 02
      , 3
      , "Man overboard"
      , chanelle
      , True
      , "N/A"
      , Nothing
      )
    , ( rsv4
      , makeTime 2021 10 05 14 44 02
      , 3
      , "Damage to propeller"
      , chanelle
      , True
      , "Bought new propeller"
      , Just 5000
      )
    , ( rsv5
      , makeTime 2021 10 03 14 44 02
      , 3
      , "Whale sighting"
      , chanelle
      , True
      , "N/A"
      , Nothing
      )
    ]
  pure ()
