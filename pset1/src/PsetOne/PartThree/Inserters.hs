{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module PsetOne.PartThree.Inserters where

import           PsetOne.PartThree.Schema
import           PsetOne.Util

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Data.Int                       ( Int32 )
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import           Data.Time                      ( LocalTime
                                                , localDay
                                                )

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

-- functions to perform insert instructions
insertSailors :: [(Text, Int32, (Integer, Int, Int))] -> IO [Sailor]
insertSailors = makeInserter sailorsTable $ \(name, rating, (yyyy, mm, dd)) ->
  Sailor default_ (val_ name) (val_ rating) (val_ $ makeDay yyyy mm dd)

insertEmployees :: [(Text, (Integer, Int, Int), Int32)] -> IO [Employee]
insertEmployees = makeInserter
  employeesTable
  (\(name, (yyyy, mm, dd), wage) ->
    Employee default_ (val_ name) (val_ $ makeDay yyyy mm dd) (val_ wage)
  )

insertBoats :: [(Text, Text, Int32)] -> IO [Boat]
insertBoats = makeInserter
  boatsTable
  (\(name, color, length) ->
    Boat default_ (val_ name) (val_ color) (val_ length)
  )

insertEquipment :: [(Text, Text, Int32, Int32)] -> IO [Equipment]
insertEquipment = makeInserter
  equipmentTable
  (\(name, dsc, count, cost) ->
    Equipment default_ (val_ name) (val_ dsc) (val_ count) (val_ cost)
  )

insertClockTimes :: [(Employee, LocalTime, Bool)] -> IO [ClockTime]
insertClockTimes = makeInserter
  clockTimesTable
  (\(employee, timestamp, clockType) -> ClockTime
    (pk $ rescopeEmployee employee)
    (val_ timestamp)
    (val_ clockType)
  )

clockIn, clockOut :: Employee -> LocalTime -> (Employee, LocalTime, Bool)
clockIn employee time = (employee, time, True)
clockOut employee time = (employee, time, False)

insertPayments
  :: [(Either Sailor SailorId, Int32, LocalTime, PaymentType)] -> IO [Payment]
insertPayments = makeInserter
  paymentsTable
  (\(sailor, cost, time, paymentType) -> Payment
    default_
    (case sailor of
      Left  sailor -> pk $ rescopeSailor sailor
      Right sid    -> val_ sid
    )
    (val_ cost)
    (val_ time)
    (val_ paymentType)
  )

-- reservation includes information about the reservation and payment
insertReservations
  :: [(Sailor, Boat, Employee, LocalTime, Int32)] -> IO [Reserves]
insertReservations entries = do
  payments <- insertPayments
    (map
      (\(sailor, _, _, time, cost) -> (Left sailor, cost, time, ReservesPayment)
      )
      entries
    )
  makeInserter
      reservesTable
      (\((sailor, boat, employee, time, _), payment) -> Reserves
        default_
        (pk $ rescopeSailor sailor)
        (pk $ rescopeBoat boat)
        (pk $ rescopeEmployee employee)
        (pk $ rescopePayment payment)
        (val_ $ localDay time)
      )
    $ zip entries payments

-- matches up holes in original maybe with the second array; example:
--     matchHoles [-2.4, 3.5, 52.0] [Just 4, Nothing, Nothing, Just 3, Just 2, Nothing]
--     -> [Just (-2.4),Nothing,Nothing,Just 3.5,Just 52.0,Nothing]
-- This is used for optional costs in the incidents table
matchHoles :: [b] -> [Maybe a] -> [Maybe b]
matchHoles noHoles holey = reverse $ snd $ foldl
  (\(noHoles, newHoley) x -> case x of
    Just _  -> (tail noHoles, Just (head noHoles) : newHoley)
    Nothing -> (noHoles, Nothing : newHoley)
  )
  (noHoles, [])
  holey

-- incidents are associated with a reservation
-- incidents may optionally create a payment with it; most of the complexity
-- in this function comes from dealing with the optional value
-- [(reservation, time, severity, description, employee, resolved, resolution, cost)]
insertIncidents
  :: [(Reserves, LocalTime, Int32, Text, Employee, Bool, Text, Maybe Int32)]
  -> IO [Incident]
insertIncidents entries = do
  payments <- insertPayments
    ( map
        (\(reservation, time, _, _, _, _, _, Just cost) ->
          (Right $ reservesSid reservation, cost, time, IncidentPayment)
        )
    $ filter (\(_, _, _, _, _, _, _, cost) -> isJust cost) entries
    )
  makeInserter
      incidentsTable
      (\((reservation, time, severity, description, employee, resolved, resolution, _), payment) ->
        Incident
          default_
          (pk $ rescopeReserves reservation)
          (val_ time)
          (val_ severity)
          (val_ description)
          (val_ resolved)
          (pk $ rescopeEmployee employee)
          (val_ resolution)
          (case payment of
            Just payment -> just_ (pk $ rescopePayment payment)
            Nothing      -> nothing_
          )
      )
    $ zip entries
    $ matchHoles payments
    $ map (\(_, _, _, _, _, _, _, cost) -> cost) entries

-- equipment sale includes information about the equipment, sailor, and payment
insertEquipmentSales
  :: [(Sailor, Equipment, Int32, LocalTime)] -> IO [EquipmentSale]
insertEquipmentSales entries = do
  payments <- insertPayments
    (map
      (\(sailor, equipment, count, time) ->
        (Left sailor, equipmentCost equipment * count, time, EquipmentPayment)
      )
      entries
    )
  makeInserter
      equipmentSalesTable
      (\((sailor, equipment, count, _), payment) -> EquipmentSale
        (pk $ rescopePayment payment)
        (pk $ rescopeEquipment equipment)
        (pk $ rescopeSailor sailor)
        (val_ count)
      )
    $ zip entries payments