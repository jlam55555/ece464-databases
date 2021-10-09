{-# LANGUAGE OverloadedStrings #-}

module PsetOne.PartThree.Fixture where

import           PsetOne.PartThree.Schema
import           PsetOne.Util

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Data.Time

-- helper to print out each item on new line;
-- query is some query that returns an (IO item) type
showLines :: (Show a) => IO [a] -> IO ()
showLines = flip (>>=) (\list -> mapM_ (putStrLn . show) list)

-- names generated from http://listofrandomnames.com
-- numbers generated from https://www.random.org/integers
insertSailors =
  let sailorsData =
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
  in  run $ runInsertReturningList $ insert sailor $ insertExpressions $ map
        (\(name, age, (yyyy, mm, dd)) -> Sailor
          default_
          (val_ name)
          (val_ age)
          (val_ (fromGregorian yyyy mm dd))
        )
        sailorsData
