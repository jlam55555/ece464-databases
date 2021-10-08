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

import           Data.Hashable
import           Data.Int
import           Data.Text
import           Data.Time

-- table schemas
data SailorT f = Sailor
  { _sailorSid    :: C f Serial
  , _sailorSname  :: C f Text
  , _sailorRating :: C f Int32
  , _sailorDob    :: C f Day
  }
  deriving Generic

data EmployeeT f = Employee
  { _employeeEid   :: C f Serial
  , _employeeEname :: C f Text
  , _employeeDob   :: C f Day
  , _employeeWage  :: C f Int
  }
  deriving Generic

data BoatT f = Boat
  { _boatBid    :: C f Serial
  , _boatBname  :: C f Text
  , _boatColor  :: C f Text
  , _boatLength :: C f Int32
  }
  deriving Generic

data ReservesT f = Reserves
  { _reservesRid :: C f Serial
  , _reservesSid :: PrimaryKey SailorT f
  , _reservesBid :: PrimaryKey BoatT f
  , _reservesEid :: PrimaryKey EmployeeT f
  , _reservesPid :: PrimaryKey PaymentT f
  , _reservesDay :: C f Day
  }
  deriving Generic

data ClockTimeT f = ClockTime
  { _clockTimeEid  :: PrimaryKey EmployeeT f
  , _clockTimeTime :: C f LocalTime
  , _clockTimeType :: C f Bool
  }
  deriving Generic

data IncidentT f = Incident
  { _incidentIid        :: C f Serial
  , _incidentRid        :: PrimaryKey ReservesT f
  , _incidentTime       :: C f LocalTime
  , _incidentSev        :: C f Int
  , _incidentDsc        :: C f Text
  , _incidentResoled    :: C f Bool
  , _incidentEid        :: PrimaryKey EmployeeT f
  , _incidentResolution :: C f Text
  , _incidentPid        :: PrimaryKey PaymentT f
  }
  deriving Generic

data EquipmentT f = Equipment
  { _equipmentEid   :: C f Serial
  , _equipmentName  :: C f Text
  , _equipmentDsc   :: C f Text
  , _equipmentCount :: C f Int
  , _equipmentCost  :: C f Int
  }
  deriving Generic

data PaymentT f = Payment
  { _paymentPid  :: C f Serial
  , _paymentSid  :: PrimaryKey SailorT f
  , _paymentCost :: C f Int
  , _paymentTime :: C f LocalTime
  , _paymentType :: C f Int
  , _paymentPaid :: C f Bool
  }
  deriving Generic

data EquipmentSaleT f = EquipmentSale
  { _equipmentSalePid   :: PrimaryKey PaymentT f
  , _equipmentSaleEqid  :: PrimaryKey EquipmentT f
  , _equipmentSaleSid   :: PrimaryKey SailorT f
  , _equipmentSaleCount :: C f Int
  }
  deriving Generic

-- type aliases, primary keys, and table instances
type Sailor = SailorT Identity
type SailorId = PrimaryKey SailorT Identity
instance Beamable SailorT
instance Table SailorT where
  data PrimaryKey SailorT f = SailorId (C f Serial)
    deriving (Generic, Beamable)
  primaryKey = SailorId . _sailorSid
deriving instance Show Sailor
deriving instance Eq Sailor
deriving instance Show SailorId
deriving instance Eq SailorId
deriving instance Hashable SailorId

type Employee = EmployeeT Identity
type EmployeeId = PrimaryKey EmployeeT Identity
instance Beamable EmployeeT
instance Table EmployeeT where
  data PrimaryKey EmployeeT f = EmployeeId (C f Serial)
    deriving (Generic, Beamable)
  primaryKey = EmployeeId . _employeeEid
deriving instance Show Employee
deriving instance Eq Employee
deriving instance Show EmployeeId
deriving instance Eq EmployeeId
deriving instance Hashable EmployeeId

type Boat = BoatT Identity
type BoatId = PrimaryKey BoatT Identity
instance Table BoatT where
  data PrimaryKey BoatT f = BoatId (C f Serial)
    deriving (Generic, Beamable)
  primaryKey = BoatId . _boatBid
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
  data PrimaryKey ReservesT f = ReservesId (C f Serial)
    deriving (Generic, Beamable)
  primaryKey = ReservesId . _reservesRid
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
  primaryKey = ClockTimeId <$> _clockTimeEid <*> _clockTimeTime
deriving instance Show ClockTime
deriving instance Eq ClockTime
deriving instance Show ClockTimeId
deriving instance Eq ClockTimeId
deriving instance Hashable ClockTimeId

type Equipment = EquipmentT Identity
type EquipmentId = PrimaryKey EquipmentT Identity
instance Beamable EquipmentT
instance Table EquipmentT where
  data PrimaryKey EquipmentT f = EquipmentId (C f Serial)
    deriving (Generic, Beamable)
  primaryKey = EquipmentId . _equipmentEid
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
  primaryKey = EquipmentSaleId . _equipmentSalePid
deriving instance Show EquipmentSale
deriving instance Eq EquipmentSale
deriving instance Show EquipmentSaleId
deriving instance Eq EquipmentSaleId
deriving instance Hashable EquipmentSaleId

type Payment = PaymentT Identity
type PaymentId = PrimaryKey PaymentT Identity
instance Beamable PaymentT
instance Table PaymentT where
  data PrimaryKey PaymentT f = PaymentId (C f Serial)
    deriving (Generic, Beamable)
  primaryKey = PaymentId . _paymentPid
deriving instance Show Payment
deriving instance Eq Payment
deriving instance Show PaymentId
deriving instance Eq PaymentId
deriving instance Hashable PaymentId

-- type synonym for serial postgres type
type Serial = SqlSerial Int32
deriving instance Generic Serial
deriving instance Hashable Serial

-- allowing times to be hashable
deriving instance Generic TimeOfDay
deriving instance Hashable TimeOfDay
deriving instance Generic Day
deriving instance Hashable Day
deriving instance Generic LocalTime
deriving instance Hashable LocalTime

-- database schema
data CompanyDb f = CompanyDb
  { _companySailors        :: f (TableEntity SailorT)
  , _companyEmployees      :: f (TableEntity EmployeeT)
  , _companyBoats          :: f (TableEntity BoatT)
  , _companyReserves       :: f (TableEntity ReservesT)
  , _companyClockTimes     :: f (TableEntity ClockTimeT)
  , _companyEquipment      :: f (TableEntity EquipmentT)
  , _companyEquipmentSales :: f (TableEntity EquipmentSaleT)
  , _companyPayments       :: f (TableEntity PaymentT)
  }
  deriving (Generic, Database be)

-- fix default naming scheme for primary keys
-- TODO: working here
companyDb :: DatabaseSettings be CompanyDb
companyDb = defaultDbSettings `withDbModification` dbModification
  { _companyReserves = modifyTableFields tableModification
                         { _reservesBid = BoatId (fieldNamed "bid")
                         , _reservesSid = SailorId (fieldNamed "sid")
                         }
  }

-- shorthands for accessing tables
type CompanyTable t s = Q Postgres CompanyDb s (t (QExpr Postgres s))

getTable
  :: (Database Postgres db, BeamSqlBackend Postgres)
  => (  DatabaseSettings Postgres CompanyDb
     -> DatabaseEntity Postgres db (TableEntity table)
     )
  -> Q Postgres db s (table (QExpr Postgres s))
getTable tableSelector = all_ $ tableSelector companyDb

sailors = getTable _companySailors
employees = getTable _companyEmployees
boats = getTable _companyBoats
reserves = getTable _companyReserves
clockTimes = getTable _companyClockTimes
equipment = getTable _companyEquipment
equipmentSales = getTable _companyEquipmentSales
payments = getTable _companyPayments
