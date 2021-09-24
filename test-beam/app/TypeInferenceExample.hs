{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeApplications, DeriveAnyClass, StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses, ImpredicativeTypes, NoExtendedDefaultRules #-}

-- following tutorial from https://haskell-beam.github.io/beam/tutorials/tutorial1/
-- minimal example for https://stackoverflow.com/q/69319523/2397327

-- need to have beam-core and beam-sqlite packages installed
import Database.Beam.Sqlite
import Database.SQLite.Simple

-- version 1
-- getDb                = open "shoppingcart1.db"
-- runDebug conn        = runBeamSqliteDebug putStr conn
-- runDebugInDb actions = do conn <- getDb
--                           runDebug conn actions

-- version 2
dbFile               = "shoppingcart1.db"
getDb                = open dbFile
runDebug conn        = runBeamSqliteDebug putStr conn
-- runDebugInDb         :: SqliteM a -> IO a
runDebugInDb actions = do conn <- getDb
                          runDebug conn actions

-- running version 1:
-- λ> :t (getDb, runDebug, runDebugInDb)
-- (getDb, runDebug, runDebugInDb)
--   :: (IO Connection, Connection -> SqliteM a -> IO a,
--       SqliteM b -> IO b)

-- running version 2:
-- λ> :t (getDb, runDebug, runDebugInDb)
-- (getDb, runDebug, runDebugInDb)
--   :: (IO Connection, Connection -> SqliteM a -> IO a,
--       SqliteM ghc-prim-0.6.1:GHC.Types.Any
--       -> IO ghc-prim-0.6.1:GHC.Types.Any)
