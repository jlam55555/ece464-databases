module PsetOne.PartThree.Fixture where

import           PsetOne.PartThree.Schema
import           PsetOne.Util

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Data.Time

a =
  let toInsert =
        [Sailor default_ "hi" 32 (val_ (fromGregorian 2021 10 14))] :: [ SailorT
              (QExpr Postgres s)
          ]
  in  run $ runInsertReturningList $ insert sailor $ insertExpressions toInsert
