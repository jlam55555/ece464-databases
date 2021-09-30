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

module Schema where

import           Util

import           Database.Beam
import           Database.Beam.MySQL
import           Database.MySQL.Simple

import           Data.Hashable
import           Data.Text
import           Data.Time

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

instance Beamable SailorT
deriving instance Show Sailor
deriving instance Eq Sailor

instance Table ReservesT where
  data PrimaryKey ReservesT f = ReservesId
    (PrimaryKey SailorT f) (PrimaryKey BoatT f) (C f Day)
    deriving (Generic, Beamable)
  primaryKey = ReservesId <$> _reservesSid <*> _reservesBid <*> _reservesDay

-- database scheme
data SailorDb f = SailorDb
  { _sailorsSailors  :: f (TableEntity SailorT)
  , _sailorsBoats    :: f (TableEntity BoatT)
  , _sailorsReserves :: f (TableEntity ReservesT)
  }
  deriving (Generic, Database be)

-- fix default naming scheme for primary keys
sailorDb :: DatabaseSettings be SailorDb
sailorDb = defaultDbSettings `withDbModification` dbModification
  { _sailorsReserves = modifyTableFields tableModification
                         { _reservesBid = BoatId (fieldNamed "bid")
                         , _reservesSid = SailorId (fieldNamed "sid")
                         }
  }

-- convenience shorthands
sailors :: Q MySQL SailorDb s (SailorT (QExpr MySQL s))
sailors = all_ $ _sailorsSailors sailorDb

boats :: Q MySQL SailorDb s (BoatT (QExpr MySQL s))
boats = all_ $ _sailorsBoats sailorDb

reserves :: Q MySQL SailorDb s (ReservesT (QExpr MySQL s))
reserves = all_ $ _sailorsReserves sailorDb

-- deriving instances
instance Beamable BoatT
deriving instance Show Boat
deriving instance Eq Boat

instance Beamable ReservesT
deriving instance Show Reserves
deriving instance Eq Reserves

deriving instance Show (PrimaryKey SailorT Identity)
deriving instance Eq (PrimaryKey SailorT Identity)
deriving instance Hashable (PrimaryKey SailorT Identity)

deriving instance Show (PrimaryKey BoatT Identity)
deriving instance Eq (PrimaryKey BoatT Identity)
deriving instance Hashable (PrimaryKey BoatT Identity)
