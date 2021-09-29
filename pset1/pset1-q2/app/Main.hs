{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import           Lib

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           Database.MySQL.Simple

import           Data.Int
import           Data.Text
import           Data.Time

main :: IO ()
main = someFunc

-- sailor schema
data SailorT f = Sailor
  { _sailorSid    :: C f Int
  , _sailorSname  :: C f Text
  , _sailorRating :: C f Int
  , _sailorAge    :: C f Int
  }
  deriving Generic

type Sailor = SailorT Identity
type SailorId = PrimaryKey SailorT Identity

instance Beamable SailorT
deriving instance Show Sailor
deriving instance Eq Sailor

instance Table SailorT where
  data PrimaryKey SailorT f = SailorId (C f Int)
    deriving (Generic, Beamable)
  primaryKey = SailorId . _sailorSid

-- boat schema
data BoatT f = Boat
  { _boatBid    :: C f Int
  , _boatBname  :: C f Text
  , _boatColor  :: C f Text
  , _boatLength :: C f Int
  }
  deriving Generic

type Boat = BoatT Identity
type BoatId = PrimaryKey BoatT Identity

instance Beamable BoatT
deriving instance Show Boat
deriving instance Eq Boat

instance Table BoatT where
  data PrimaryKey BoatT f = BoatId (C f Int)
    deriving (Generic, Beamable)
  primaryKey = BoatId . _boatBid

-- reserves schema
data ReservesT f = Reserves
  { _reservesSid :: PrimaryKey SailorT f
  , _reservesBid :: PrimaryKey BoatT f
  , _reservesDay :: C f Day
  }
  deriving Generic

type Reserves = ReservesT Identity
type ReservesId = PrimaryKey ReservesT Identity

instance Beamable ReservesT
deriving instance Show Reserves
deriving instance Eq Reserves

deriving instance Show (PrimaryKey SailorT Identity)
deriving instance Eq (PrimaryKey SailorT Identity)

deriving instance Show (PrimaryKey BoatT Identity)
deriving instance Eq (PrimaryKey BoatT Identity)

instance Table ReservesT where
  data PrimaryKey ReservesT f = ReservesId
    (PrimaryKey SailorT f) (PrimaryKey BoatT f) (C f Day)
    deriving (Generic, Beamable)
  primaryKey = ReservesId <$> _reservesSid <*> _reservesBid <*> _reservesDay

data SailorDb f = SailorDb
  { _sailorsSailors  :: f (TableEntity SailorT)
  , _sailorsBoats    :: f (TableEntity BoatT)
  , _sailorsReserves :: f (TableEntity ReservesT)
  }
  deriving (Generic, Database be)

-- not sure why this isn't implemented; allows us to use the `Day` instance
instance FromBackendRow MySQL Day

sailorDb :: DatabaseSettings be SailorDb
sailorDb = defaultDbSettings `withDbModification` dbModification
  { _sailorsReserves = modifyTableFields tableModification
                         { _reservesBid = BoatId (fieldNamed "bid")
                         , _reservesSid = SailorId (fieldNamed "sid")
                         }
  }

sailors = all_ $ _sailorsSailors sailorDb
boats = all_ $ _sailorsBoats sailorDb
reserves = all_ $ _sailorsReserves sailorDb

dbConnection = connect defaultConnectInfo { connectUser     = "jon"
                                          , connectDatabase = "ece464_pset1"
                                          }

-- abstracted syntax for joins; more consistent than the library `join_`
join tab1 tab2 cond res = do
  res1 <- tab1
  res2 <- tab2
  guard_ $ cond res1 res2
  pure $ res res1 res2

runQuery query =
  dbConnection >>= \conn -> runBeamMySQLDebug putStrLn conn query

-- query #1 -- version 2
pset1Query1 = runQuery $ runSelectReturningList $ select $ join
  boats
  ( aggregate_
      (\(boat, reservation) -> (group_ (_boatBid boat), as_ @Int countAll_))
  $ join boats
         reserves
         (\boat reservation -> _reservesBid reservation `references_` boat)
         (,)
  )
  (\boat (bid, count) -> BoatId bid `references_` boat)
  (\boat (bid, count) -> (bid, (_boatBname boat), count))

-- query #2
pset1Query2 = runQuery $ runSelectReturningList $ select $ do
  sailor <- filter_
    (\sailor -> not_ . exists_ $ filter_
      (\boat ->
        (_boatColor boat ==. val_ "red")
          &&. (not_ . exists_ $ filter_
                (\reservation ->
                  (_reservesBid reservation `references_` boat)
                    &&. (_reservesSid reservation `references_` sailor)
                )
                reserves
              )
      )
      boats
    )
    sailors
  pure (_sailorSid sailor, _sailorSname sailor)

-- query #3
pset1Query3 = runQuery $ runSelectReturningList $ select $ join
  sailors
  (let haveReservedRed = join
         reserves
         (filter_ (\boat -> _boatColor boat ==. val_ "red") boats)
         (\reservation boat -> _reservesBid reservation `references_` boat)
         (\reservation boat -> (_reservesSid reservation))
       haveReservedNonRed = join
         reserves
         (filter_ (\boat -> _boatColor boat /=. val_ "red") boats)
         (\reservation boat -> _reservesBid reservation `references_` boat)
         (\reservation boat -> (_reservesSid reservation))
   in  haveReservedRed `except_` haveReservedNonRed
  )
  (\sailor (SailorId sid) -> sid ==. _sailorSid sailor)
  (\sailor (SailorId sid) -> (sid, _sailorSname sailor))

-- query #4
pset1Query4 = runQuery $ do
  runSelectReturningList
    $ select
    $ (let reservesByBoat = aggregate_
             (\reservation ->
               (group_ (_reservesBid reservation), as_ @Int countAll_)
             )
             reserves
           reservesByBoat2 = aggregate_
             (\reservation ->
               (group_ (_reservesBid reservation), as_ @Int countAll_)
             )
             reserves
           maximumReservesByBoat = filter_
             (\(_, _, count) -> count ==. fromMaybe_
               0
               ( subquery_
               $ aggregate_ (\(_, count) -> max_ count) reservesByBoat
               )
             )
             (join reservesByBoat2
                   boats
                   (\(bid, count) boat -> bid `references_` boat)
                   (\(bid, count) boat -> (bid, _boatBname boat, count))
             )
       in  maximumReservesByBoat
      )

-- query #5
pset1Query5 = runQuery $ runSelectReturningList $ select $ do
  sailor <- nub_ $ except_
    sailors
    (join
      (filter_ (\boat -> _boatColor boat ==. val_ "red") boats)
      (join
        sailors
        reserves
        (\sailor reservation -> _reservesSid reservation `references_` sailor)
        (,)
      )
      (\boat (_, reservation) -> _reservesBid reservation `references_` boat)
      (\_ (sailor, _) -> sailor)
    )
  pure (_sailorSid sailor, _sailorSname sailor)

-- query #6
pset1Query6 =
  runQuery
    $ runSelectReturningList
    $ select
    -- need to cast `age` to fp type otherwise it would return an int
    $ aggregate_ (\sailor -> avg_ $ cast_ (_sailorAge sailor) double)
    $ filter_ (\sailor -> _sailorRating sailor ==. val_ 10) sailors

-- query #7
pset1Query7 = runQuery $ runSelectReturningList $ select $ join
  sailors
  (aggregate_
    (\sailor -> (group_ (_sailorRating sailor), min_ (_sailorAge sailor)))
    sailors
  )
  (\sailor (rating, age) ->
    (_sailorRating sailor ==. rating)
      &&. (_sailorAge sailor ==. fromMaybe_ (-1) age)
  )
  (\sailor _ ->
    ( _sailorSid sailor
    , _sailorSname sailor
    , _sailorRating sailor
    , _sailorAge sailor
    )
  )

-- query #8
pset1Query8 = do
  runQuery
    $ runSelectReturningList
    $ select
    $ (let
         reservationCountByBoat =
           aggregate_
               (\(sid, bid) -> (group_ bid, group_ sid, as_ @Int countAll_))
             $ join
                 boats
                 reserves
                 (\boat reservation ->
                   _reservesBid reservation `references_` boat
                 )
                 (\boat reservation ->
                   (_reservesSid reservation, _reservesBid reservation)
                 )
         maxReservationsByBoat = aggregate_
           (\(bid, sid, count) -> (group_ bid, max_ count))
           reservationCountByBoat
         reservationCountByBoat2 =
           aggregate_
               (\(sid, bid) -> (group_ bid, group_ sid, as_ @Int countAll_))
             $ join
                 boats
                 reserves
                 (\boat reservation ->
                   _reservesBid reservation `references_` boat
                 )
                 (\boat reservation ->
                   (_reservesSid reservation, _reservesBid reservation)
                 )
         maxReservationsByBoatUsers = join
           maxReservationsByBoat
           reservationCountByBoat2
           (\(bid1, count1) (bid2, _, count2) ->
             (bid1 ==. bid2) &&. (fromMaybe_ 0 count1 ==. count2)
           )
           (\_ a -> a)
         maxReservationsByBoatUserDetails = join
           maxReservationsByBoatUsers
           sailors
           (\(bid, sid, count) sailor -> sid `references_` sailor)
           (\(bid, sid, count) sailor -> (bid, sid, _sailorSname sailor, count))
       in
         maxReservationsByBoatUserDetails
      )
