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

dbConnection = connect defaultConnectInfo { connectUser     = "jon"
                                          , connectDatabase = "ece464_pset1"
                                          }

runQuery query = do
  conn <- dbConnection
  runBeamMySQLDebug putStrLn conn query

-- query #1
pset1Query1 = runQuery $ do
  runSelectReturningList $ select $ do
    (bid, count) <-
      aggregate_
          (\(boat, reservation) -> (group_ (_boatBid boat), as_ @Int countAll_))
        $ do
            boat        <- all_ (_sailorsBoats sailorDb)
            reservation <- all_ (_sailorsReserves sailorDb)
            guard_ (_reservesBid reservation `references_` boat)
            pure (boat, reservation)
    boat <- all_ (_sailorsBoats sailorDb)
    guard_ (BoatId bid `references_` boat)
    pure (bid, _boatBname boat, count)

-- query #6
pset1Query6 = runQuery $ do
  runSelectReturningList
    $ select
    -- need to cast `age` to fp type otherwise it would return an int
    $ aggregate_ (\sailor -> avg_ $ cast_ (_sailorAge sailor) double)
    $ filter_ (\sailor -> _sailorRating sailor ==. val_ 10)
    $ all_ (_sailorsSailors sailorDb)
