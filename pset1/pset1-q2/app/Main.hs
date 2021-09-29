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
import           Database.Beam.MySQL
import           Database.MySQL.Simple

import           Data.Text

main :: IO ()
main = someFunc

data SailorT f =
  Sailor
    { _sailorSid    :: C f Int
    , _sailorSname  :: C f Text
    , _sailorRating :: C f Int
    , _sailorAge    :: C f Int
    }
  deriving (Generic)

type Sailor = SailorT Identity
type SailorId = PrimaryKey SailorT Identity
instance Beamable SailorT

deriving instance Show Sailor
deriving instance Eq Sailor

instance Table SailorT where
  data PrimaryKey SailorT f = SailorId (Columnar f Int)
                              deriving (Generic, Beamable)
  primaryKey = SailorId . _sailorSid

data SailorDb f =
  SailorDb
    { _sailorsSailors :: f (TableEntity SailorT)
    }
  deriving (Generic, Database be)

sailorDb :: DatabaseSettings be SailorDb
sailorDb = defaultDbSettings

-- data UserT f =
--   User
--     { _userEmail     :: Columnar f Text
--     , _userFirstName :: Columnar f Text
--     , _userLastName  :: Columnar f Text
--     , _userPassword  :: Columnar f Text
--     }
--   deriving (Generic)

-- type User = UserT Identity

-- type UserId = PrimaryKey UserT Identity

-- deriving instance Show User

-- deriving instance Eq User

-- instance Beamable UserT

-- instance Table UserT where
--   data PrimaryKey UserT f = UserId (Columnar f Text)
--                             deriving (Generic, Beamable)
--   primaryKey = UserId . _userEmail

-- data ShoppingCartDb f =
--   ShoppingCartDb
--     { _shoppingCartUsers :: f (TableEntity UserT)
--     }
--   deriving (Generic, Database be)

-- shoppingCartDb :: DatabaseSettings be ShoppingCartDb
-- shoppingCartDb = defaultDbSettings

dbConnection =
  connect defaultConnectInfo {connectUser = "jon", connectDatabase = "ece464_pset1"}

-- test query
testQuery = do
  conn <- dbConnection
  runBeamMySQLDebug putStr conn $ do
    runSelectReturningList $ select $ all_ (_sailorsSailors sailorDb)
