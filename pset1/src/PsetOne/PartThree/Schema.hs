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

module PsetOne.PartThree.Schema where

import           PsetOne.Util

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.Types
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple

import           Control.Exception

import           Data.Hashable
import           Data.Int
import           Data.Text
import           Data.Time

-- table schemas
data SailorT f = Sailor
  { sailorSid    :: C f Int32
  , sailorSname  :: C f Text
  , sailorRating :: C f Int32
  , sailorDob    :: C f Day
  }
  deriving Generic

data EmployeeT f = Employee
  { employeeEid   :: C f Int32
  , employeeEname :: C f Text
  , employeeDob   :: C f Day
  , employeeWage  :: C f Int32
  }
  deriving Generic

data BoatT f = Boat
  { boatBid    :: C f Int32
  , boatBname  :: C f Text
  , boatColor  :: C f Text
  , boatLength :: C f Int32
  }
  deriving Generic

data ReservesT f = Reserves
  { reservesRid :: C f Int32
  , reservesSid :: PrimaryKey SailorT f
  , reservesBid :: PrimaryKey BoatT f
  , reservesEid :: PrimaryKey EmployeeT f
  , reservesPid :: PrimaryKey PaymentT f
  , reservesDay :: C f Day
  }
  deriving Generic

data ClockTimeT f = ClockTime
  { clocktimeEid  :: PrimaryKey EmployeeT f
  , clocktimeTime :: C f LocalTime
  , clocktimeType :: C f Bool
  }
  deriving Generic

data IncidentT f = Incident
  { incidentIid        :: C f Int32
  , incidentRid        :: PrimaryKey ReservesT f
  , incidentTime       :: C f LocalTime
  , incidentSev        :: C f Int32
  , incidentDsc        :: C f Text
  , incidentResoled    :: C f Bool
  , incidentEid        :: PrimaryKey EmployeeT f
  , incidentResolution :: C f Text
  , incidentPid        :: PrimaryKey PaymentT f
  }
  deriving Generic

data EquipmentT f = Equipment
  { equipmentEid   :: C f Int32
  , equipmentName  :: C f Text
  , equipmentDsc   :: C f Text
  , equipmentCount :: C f Int32
  , equipmentCost  :: C f Int32
  }
  deriving Generic

data PaymentT f = Payment
  { paymentPid  :: C f Int32
  , paymentSid  :: PrimaryKey SailorT f
  , paymentCost :: C f Int32
  , paymentTime :: C f LocalTime
  , paymentType :: C f Int32
  }
  deriving Generic

data EquipmentSaleT f = EquipmentSale
  { equipmentsalePid   :: PrimaryKey PaymentT f
  , equipmentsaleEqid  :: PrimaryKey EquipmentT f
  , equipmentsaleSid   :: PrimaryKey SailorT f
  , equipmentsaleCount :: C f Int32
  }
  deriving Generic

-- type aliases, primary keys, and table instances
type Sailor = SailorT Identity
type SailorId = PrimaryKey SailorT Identity
instance Beamable SailorT
instance Table SailorT where
  data PrimaryKey SailorT f = SailorId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = SailorId . sailorSid
deriving instance Show Sailor
deriving instance Eq Sailor
deriving instance Show SailorId
deriving instance Eq SailorId
deriving instance Hashable SailorId

type Employee = EmployeeT Identity
type EmployeeId = PrimaryKey EmployeeT Identity
instance Beamable EmployeeT
instance Table EmployeeT where
  data PrimaryKey EmployeeT f = EmployeeId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = EmployeeId . employeeEid
deriving instance Show Employee
deriving instance Eq Employee
deriving instance Show EmployeeId
deriving instance Eq EmployeeId
deriving instance Hashable EmployeeId

type Boat = BoatT Identity
type BoatId = PrimaryKey BoatT Identity
instance Table BoatT where
  data PrimaryKey BoatT f = BoatId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = BoatId . boatBid
instance Beamable BoatT
deriving instance Show Boat
deriving instance Eq Boat
deriving instance Show BoatId
deriving instance Eq BoatId
deriving instance Hashable BoatId

type Reserves = ReservesT Identity
type ReservesId = PrimaryKey ReservesT Identity
instance Beamable ReservesT
instance Table ReservesT where
  data PrimaryKey ReservesT f = ReservesId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = ReservesId . reservesRid
deriving instance Show Reserves
deriving instance Eq Reserves
deriving instance Show ReservesId
deriving instance Eq ReservesId
deriving instance Hashable ReservesId

type ClockTime = ClockTimeT Identity
type ClockTimeId = PrimaryKey ClockTimeT Identity
instance Beamable ClockTimeT
instance Table ClockTimeT where
  data PrimaryKey ClockTimeT f = ClockTimeId (PrimaryKey EmployeeT f) (C f LocalTime)
    deriving (Generic, Beamable)
  primaryKey = ClockTimeId <$> clocktimeEid <*> clocktimeTime
deriving instance Show ClockTime
deriving instance Eq ClockTime
deriving instance Show ClockTimeId
deriving instance Eq ClockTimeId
deriving instance Hashable ClockTimeId

type Incident = IncidentT Identity
type IncidentId = PrimaryKey IncidentT Identity
instance Beamable IncidentT
instance Table IncidentT where
  data PrimaryKey IncidentT f = IncidentId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = IncidentId . incidentIid
deriving instance Show Incident
deriving instance Eq Incident
deriving instance Show IncidentId
deriving instance Eq IncidentId
deriving instance Hashable IncidentId

type Equipment = EquipmentT Identity
type EquipmentId = PrimaryKey EquipmentT Identity
instance Beamable EquipmentT
instance Table EquipmentT where
  data PrimaryKey EquipmentT f = EquipmentId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = EquipmentId . equipmentEid
deriving instance Show Equipment
deriving instance Eq Equipment
deriving instance Show EquipmentId
deriving instance Eq EquipmentId
deriving instance Hashable EquipmentId

type EquipmentSale = EquipmentSaleT Identity
type EquipmentSaleId = PrimaryKey EquipmentSaleT Identity
instance Beamable EquipmentSaleT
instance Table EquipmentSaleT where
  data PrimaryKey EquipmentSaleT f = EquipmentSaleId (PrimaryKey PaymentT f)
    deriving (Generic, Beamable)
  primaryKey = EquipmentSaleId . equipmentsalePid
deriving instance Show EquipmentSale
deriving instance Eq EquipmentSale
deriving instance Show EquipmentSaleId
deriving instance Eq EquipmentSaleId
deriving instance Hashable EquipmentSaleId

type Payment = PaymentT Identity
type PaymentId = PrimaryKey PaymentT Identity
instance Beamable PaymentT
instance Table PaymentT where
  data PrimaryKey PaymentT f = PaymentId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = PaymentId . paymentPid
deriving instance Show Payment
deriving instance Eq Payment
deriving instance Show PaymentId
deriving instance Eq PaymentId
deriving instance Hashable PaymentId

-- allowing times to be hashable
deriving instance Generic TimeOfDay
deriving instance Hashable TimeOfDay
deriving instance Generic Day
deriving instance Hashable Day
deriving instance Generic LocalTime
deriving instance Hashable LocalTime

-- database schema
data CompanyDb f = CompanyDb
  { companySailors        :: f (TableEntity SailorT)
  , companyEmployees      :: f (TableEntity EmployeeT)
  , companyBoats          :: f (TableEntity BoatT)
  , companyReserves       :: f (TableEntity ReservesT)
  , companyClockTimes     :: f (TableEntity ClockTimeT)
  , companyIncidents      :: f (TableEntity IncidentT)
  , companyEquipment      :: f (TableEntity EquipmentT)
  , companyEquipmentSales :: f (TableEntity EquipmentSaleT)
  , companyPayments       :: f (TableEntity PaymentT)
  }
  deriving (Generic, Database be)

-- default naming scheme for primary keys is weird; don't use it
-- TODO: working here
companyDb :: DatabaseSettings be CompanyDb
companyDb = defaultDbSettings `withDbModification` dbModification
  { companyReserves       = modifyTableFields tableModification
                              { reservesBid = BoatId "bid"
                              , reservesSid = SailorId "sid"
                              , reservesEid = EmployeeId "eid"
                              , reservesPid = PaymentId "pid"
                              }
  , companyClockTimes     = modifyTableFields tableModification
                              { clocktimeEid = EmployeeId "eid"
                              }
  , companyIncidents      = modifyTableFields tableModification
                              { incidentRid = ReservesId "rid"
                              , incidentEid = EmployeeId "eid"
                              , incidentPid = PaymentId "pid"
                              }
  , companyEquipmentSales = modifyTableFields tableModification
                              { equipmentsalePid  = PaymentId "pid"
                              , equipmentsaleEqid = EquipmentId "eqid"
                              , equipmentsaleSid  = SailorId "sid"
                              }
  , companyPayments       = modifyTableFields tableModification
                              { paymentSid = SailorId "sid"
                              }
  }

-- shorthands for accessing tables and connection
getTable
  :: (Database Postgres CompanyDb, BeamSqlBackend Postgres)
  => (  DatabaseSettings Postgres CompanyDb
     -> DatabaseEntity Postgres CompanyDb (TableEntity table)
     )
  -> ( DatabaseEntity Postgres CompanyDb (TableEntity table)
     , Q Postgres CompanyDb s (table (QExpr Postgres s))
     )
getTable tableSelector =
  (tableSelector companyDb, all_ $ tableSelector companyDb)

(sailorsTable, sailors) = getTable companySailors
(employeesTable, employees) = getTable companyEmployees
(boatsTable, boats) = getTable companyBoats
(reservesTable, reserves) = getTable companyReserves
(clockTimesTable, clockTimes) = getTable companyClockTimes
(equipmentTable, equipment) = getTable companyEquipment
(equipmentSalesTable, equipmentSales) = getTable companyEquipmentSales
(paymentsTable, payments) = getTable companyPayments

-- database connection object
conn = getDbConn "ece464_pset1_part3"

-- helper to run queries
run = runQuery conn

-- set up and destroy the tables in the database
setupSchema = runSqlFile conn "res/pset1_part3_setup.sql"
cleanupSchema = runSqlFile conn "res/pset1_part3_cleanup.sql"

-- reset schema; delete and then recreate tables
-- (if tables do not exist, simply create them)
resetSchema = do
  result <- try cleanupSchema
  case (result :: Either SqlError Int64) of
    Right _   -> setupSchema
    Left  err -> setupSchema
