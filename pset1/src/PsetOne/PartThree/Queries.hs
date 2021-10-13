{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PsetOne.PartThree.Queries where

import           PsetOne.PartThree.Schema
import           PsetOne.Util

import           Database.Beam

import           Data.Int                       ( Int32 )

runSelect query = run $ runSelectReturningList $ select query
runSelectOne query = run $ runSelectReturningOne $ select query

-- query all records from a table
querySailors = runSelect sailors
queryEmployees = runSelect employees
queryBoats = runSelect boats

-- helper functions
isRed b = boatColor b ==. val_ "red"
redBs = filter_ isRed boats
nonRedBs = filter_ (not_ . isRed) boats
joinOnBid b r = reservesBid r `references_` b
joinOnSid s r = reservesSid r `references_` s

-- get sailors who have reserved all red boats
querySailorsReservedAllRedBoats = runSelect $ do
  s <- filter_
    (\s -> not_ . exists_ $ filter_
      (\b ->
        isRed b
          &&. (not_ . exists_ $ filter_
                (\r -> joinOnBid b r &&. joinOnSid s r)
                reserves
              )
      )
      boats
    )
    sailors
  pure (pk s, sailorSname s)

-- get sailors who have reserved only red boats
querySailorsReservedOnlyRedBoats = runSelect $ join
  sailors
  (let haveReservedRed = join redBs reserves joinOnBid (\_ r -> reservesSid r)
       haveReservedNonRed =
         join nonRedBs reserves joinOnBid (\_ r -> reservesSid r)
   in  haveReservedRed `except_` haveReservedNonRed
  )
  (\s sid -> sid `references_` s)
  (\s sid -> (sid, sailorSname s))

-- get sailors along with how much they have spent
querySailorsSpending = do
  spendings <- runSelect $ join
    (aggregate_
      (\p -> (group_ (paymentSid p), as_ @(Maybe Int32) $ sum_ (paymentCost p)))
      payments
    )
    sailors
    (\(sid, _) s -> sid `references_` s)
    (\(sid, spent) s -> (sid, sailorSname s, fromMaybe_ 0 spent))
  pure $ map
    (\(sid, sname, spent) -> (sid, sname, fromIntegral spent / 100.0))
    spendings

-- get all pairs of sailors and employees who have met through some transaction
-- (i.e., through reservations or incidents)
querySailorsServicedEmployees =
  runSelect
    $ orderBy_ (\(EmployeeId eid, _, SailorId sid, _) -> (asc_ eid, desc_ sid))
    $ nub_
    $ union_
        (join
          employees
          (join
            sailors
            (join incidents reserves (\i r -> incidentRid i `references_` r) (,)
            )
            (\s (_, r) -> reservesSid r `references_` s)
            (,)
          )
          (\e (_, (i, _)) -> incidentEid i `references_` e)
          (\e (s, _) -> (pk e, employeeEname e, pk s, sailorSname s))
        )
        (join employees
              (join sailors reserves joinOnSid (,))
              (\e (_, r) -> reservesEid r `references_` e)
              (\e (s, _) -> (pk e, employeeEname e, pk s, sailorSname s))
        )

-- get the sailor who has bought the most boat hook ends
querySailorMostBoatHookEnds = run $ runSelectReturningList $ selectWith $ do
  sailorsBoatHookEnds <-
    selecting
    $ aggregate_
        (\(sid, sname, count) ->
          (group_ sid, group_ sname, fromMaybe_ 0 $ sum_ count)
        )
    $ join
        sailors
        (filter_
          (\es -> equipmentsaleEqid es ==. EquipmentId
            (   subquery_
            $   (filter_ (\eq -> equipmentName eq ==. "Boat Hook End") equipment
                )
            >>= pure
            .   \eq -> equipmentEqid eq
            )
          )
          equipmentSales
        )
        (\s es -> equipmentsaleSid es `references_` s)
        (\s es -> (pk s, sailorSname s, equipmentsaleCount es))
  pure
    $ filter_
        (\(_, _, count) -> count ==. subquery_
          ( aggregate_ (\(_, _, count) -> fromMaybe_ 0 $ max_ count)
          $ reuse sailorsBoatHookEnds
          )
        )
    $ reuse sailorsBoatHookEnds

