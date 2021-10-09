{-# LANGUAGE OverloadedStrings #-}

module PsetOne.PartThree.Fixture where

import           PsetOne.PartThree.Schema
import           PsetOne.Util

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Data.Int
import           Data.Text                      ( Text )
import           Data.Time

-- helper to print out each item on new line;
-- query is some query that returns an (IO item) type
showLines :: (Show a) => IO [a] -> IO ()
showLines = flip (>>=) (\list -> mapM_ (putStrLn . show) list)

-- names generated from http://listofrandomnames.com
-- numbers generated from https://www.random.org/integers
insertSailors :: [(Text, Int32, (Integer, Int, Int))] -> IO [Sailor]
insertSailors sailorsData =
  run $ runInsertReturningList $ insert sailorsTable $ insertExpressions $ map
    (\(name, rating, (yyyy, mm, dd)) -> Sailor
      default_
      (val_ name)
      (val_ rating)
      (val_ (fromGregorian yyyy mm dd))
    )
    sailorsData

insertEmployees :: [(Text, (Integer, Int, Int), Int32)] -> IO [Employee]
insertEmployees employeesData =
  run $ runInsertReturningList $ insert employeesTable $ insertExpressions $ map
    (\(name, (yyyy, mm, dd), wage) -> Employee
      default_
      (val_ name)
      (val_ (fromGregorian yyyy mm dd))
      (val_ wage)
    )
    employeesData

createFixture = do
  resetSchema
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
  pure newSailors
