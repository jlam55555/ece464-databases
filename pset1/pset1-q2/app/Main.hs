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

-- abstracted syntax for joins
join tab1 tab2 cond res = do
  res1 <- tab1
  res2 <- tab2
  guard_ $ cond res1 res2
  pure $ res res1 res2

runQuery query = do
  conn <- dbConnection
  runBeamMySQLDebug putStrLn conn query

-- query #1 -- version 2
pset1Query1 = runQuery $ do
  runSelectReturningList $ select $ join
    boats
    ( aggregate_
        (\(boat, reservation) -> (group_ (_boatBid boat), as_ @Int countAll_))
    $ join
        boats
        reserves
        (\boat reservation -> _reservesBid reservation `references_` boat)
        (,)
    )
    (\boat (bid, count) -> BoatId bid `references_` boat)
    (\boat (bid, count) -> (bid, (_boatBname boat), count))

-- pset1Query2 = runQuery $ do
--   runSelectReturningList $ select $ filter_
--     (\sailor -> not_ . exists_ $ except_ boats boats
--       -- (fmap
--       --   (\reservation -> let BoatId bid = _reservesBid reservation in bid)
--       --   (filter_
--       --     (\reservation -> _reservesSid reservation `references_` sailor)
--       --     reserves
--       --   )
--       -- )
--                                                     )
--     sailors

-- outer = filter_ (\sailor -> (not_ . exists_) (middle sailor)) sailors

-- -- SELECT sid, sname
-- -- FROM sailors s
-- -- WHERE NOT EXISTS middle

-- middle s =
--   except_ (fmap (BoatId . _boatBid) boats) (fmap _reservesBid (inner s))

-- -- SELECT bid
-- -- FROM boats
-- -- EXCEPT inner

-- inner s =
--   filter_ (\reservation -> _reservesSid reservation `references_` s) reserves

-- SELECT bid
-- FROM reserves r
-- WHERE r.sid=s.sid

-- SELECT sid, sname
-- FROM sailors s
-- WHERE NOT EXISTS (
--       -- red boat not reserved by them
--       SELECT bid
--       FROM boats
--       EXCEPT (
--              SELECT bid
--              FROM reserves r
--              WHERE r.sid=s.sid
--       )
-- );


-- query #6
pset1Query6 = runQuery $ do
  runSelectReturningList
    $ select
    -- need to cast `age` to fp type otherwise it would return an int
    $ aggregate_ (\sailor -> avg_ $ cast_ (_sailorAge sailor) double)
    $ filter_ (\sailor -> _sailorRating sailor ==. val_ 10) sailors
