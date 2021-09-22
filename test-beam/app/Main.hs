{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeApplications, DeriveAnyClass, StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses #-}

-- haskell keybindings (make sure to turn on haskell-indent-mode)
-- https://wiki.haskell.org/Emacs/Keybindings_and_simple_usage

module Main where

import Lib

import Database.Beam
import Database.Beam.Sqlite
import Data.Int

import Data.Text (Text)

import Database.SQLite.Simple


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

getDb = open "shoppingcart1.db"
runDebug conn = runBeamSqliteDebug putStr conn
runDebugInDb actions = do conn <- getDb
                          runDebug conn actions
userTable = _shoppingCartUsers shoppingCartDb

showUsers2 = let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
             in runDebugInDb $ do users <- runSelectReturningList $ select allUsers
                                  mapM_ (liftIO . putStrLn . show) users

showUsers3 = let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
             in runDebugInDb $
                (runSelectReturningList $ select allUsers) >>=
                mapM_ (liftIO . putStrLn . show)

addUsers = do conn <- getDb
              runDebug conn $ runInsert $
                insert (_shoppingCartUsers shoppingCartDb) $
                insertValues [ User "js@test.com" "James" "Smith" "1332123"
                             , User "bj@test.com" "Betty" "Jones" "1232132" ]

showUsers = let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
            in do conn <- getDb
                  runDebug conn $
                    do users <- runSelectReturningList $ select allUsers
                       mapM_ (liftIO . putStrLn . show) users
                
showUsersSortByFirstName
  = let sortUsersByFirstName =
          orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) $
          all_ (_shoppingCartUsers shoppingCartDb)
    in do conn <- getDb
          runDebug conn $ do
            users <- runSelectReturningList $ select sortUsersByFirstName
            mapM_ (liftIO . putStrLn . show) users

showUsersBounded = runDebugInDb $ do users <- runSelectReturningList $
                                       select $
                                       limit_ 1 $
                                       offset_ 1 $
                                       orderBy_ (asc_ . _userFirstName) $
                                       all_ (_shoppingCartUsers shoppingCartDb)
                                     mapM_ (liftIO . putStrLn . show) users
                                     
showUserCount = runDebugInDb $ do c <- runSelectReturningOne $
                                    select $
                                    aggregate_ (\u -> as_ @Int32 countAll_) $
                                    all_ userTable
                                  liftIO $ putStrLn ("We have " ++ show c ++ " users in the db.")
