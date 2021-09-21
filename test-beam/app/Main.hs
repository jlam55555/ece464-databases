{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeApplications, DeriveAnyClass, StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses #-}

-- haskell keybindings (make sure to turn on haskell-indent-mode)
-- https://wiki.haskell.org/Emacs/Keybindings_and_simple_usage

module Main where

import Lib

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)

-- sample thing to play around with
-- let range = [0,4..1000] in map (\(x, y) -> y) (filter (\(x, y) -> x) (zip (map isLeapYear range) range))

main :: IO ()
main = someFunc

data UserT f = User
    {
      _userEmail     :: Columnar f Text,
      _userFirstName :: Columnar f Text,
      _userLastName  :: Columnar f Text,
      _userPassword  :: Columnar f Text
    } deriving Generic

type User   = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey              = UserId . _userEmail

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                      deriving (Generic, Database be)

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings
