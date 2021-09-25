{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeApplications, DeriveAnyClass, StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses, ImpredicativeTypes, UndecidableInstances #-}

-- haskell keybindings (make sure to turn on haskell-indent-mode)
-- https://wiki.haskell.org/Emacs/Keybindings_and_simple_usage

-- TODO: look at structured haskell mode

module Main where

import Lib

import Database.Beam
import Database.Beam.Sqlite
import Data.Int

import Data.Text (Text)

import Database.SQLite.Simple

-- for debugging
import Data.Typeable

-- for lenses
import Control.Lens

import System.Process (callCommand)
import Data.Text (pack, unpack, replace)

import Data.Time

import Database.Beam.Backend.SQL

-- sample thing to play around with
-- let range = [0,4..1000] in map (\(x, y) -> y) (filter (\(x, y) -> x) (zip (map isLeapYear range) range))

-- the following are equivalent:
-- [(x,y) | x <- [1,2,3], y <- [4,5,6]]
-- do x <- [1,2,3]; y <- [4,5,6]; return (x, y)
-- [1, 2, 3] >>= \x -> [4, 5, 6] >>= \y -> return (x, y)x

main :: IO ()
main = someFunc

data UserT f = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text }
  deriving Generic

type User   = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey              = UserId . _userEmail

-- data ShoppingCartDb f = ShoppingCartDb
--   { _shoppingCartUsers         :: f (TableEntity UserT)
--   , _shoppingCartUserAddresses :: f (TableEntity AddressT) }
--   deriving (Generic, Database be)

-- shoppingCartDb :: DatabaseSettings be ShoppingCartDb
-- shoppingCartDb = defaultDbSettings

-- renaming the addresses table
-- shoppingCartDb :: DatabaseSettings be ShoppingCartDb
-- shoppingCartDb = defaultDbSettings `withDbModification` dbModification
--   { _shoppingCartUserAddresses = setEntityName "addresses" <>
--     modifyTableFields tableModification
--     { _addressLine1   = fieldNamed "address1"
--     , _addressLine2   = fieldNamed "address2"
--     , _addressForUser = UserId (fieldNamed "user") } }

dbFile               = "shoppingcart1.db"
getDb                = open dbFile
runDebug conn        = runBeamSqliteDebug putStr conn
runDebugInDb         :: SqliteM a -> IO a
runDebugInDb actions = do conn <- getDb
                          runDebug conn actions
userTable            = _shoppingCartUsers shoppingCartDb

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
                insertValues [ User "james@example.com" "James" "Smith" "1332123"
                             , User "betty@example.com" "Betty" "Jones" "1232132"
                             , User "sam@example.com" "Sam" "Taylor" "123123"
                             , User "james@pallo.com" "James" "Pollo" "123213"
                             , User "betty@sims.com" "Betty" "Sims" "321332"
                             , User "james@oreily.com" "James" "O'Reily" "132421"
                             , User "sam@sophitz.com" "Sam" "Sophitz" "234121"
                             , User "sam@jely.com" "Sam" "Jely" "421552" ]

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

deleteUsers = runDebugInDb $ do runDelete $ delete userTable $ const $ val_ True

aggregateSelect = runDebugInDb $
  do countedByName <- runSelectReturningList $
       select $
       aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int32 countAll_)) $
       all_ userTable
     mapM_ (liftIO . putStrLn . show) countedByName

data AddressT f = Address
  { _addressId      :: C f Int32
  , _addressLine1   :: C f Text
  , _addressLine2   :: C f (Maybe Text)
  , _addressCity    :: C f Text
  , _addressState   :: C f Text
  , _addressZip     :: C f Text
  , _addressForUser :: PrimaryKey UserT f }
  deriving (Generic, Beamable)

type Address = AddressT Identity

deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
  data PrimaryKey AddressT f = AddressId (C f Int32) deriving (Generic, Beamable)
  primaryKey = AddressId . _addressId

type AddressId = PrimaryKey AddressT Identity

Address
  (LensFor addressId)
  (LensFor addressLine1)
  (LensFor addressLine2)
  (LensFor addressCity)
  (LensFor addressState)
  (LensFor addressZip)
  (UserId (LensFor addressForUserId))
  = tableLenses

User
  (LensFor userEmail)
  (LensFor userFirstName)
  (LensFor userLastName)
  (LensFor userPassword)
  = tableLenses

-- ShoppingCartDb
--   (TableLens shoppingCartUsers)
--   (TableLens shoppingCartUserAddresses)
--   = dbLenses

-- see: https://github.com/haskell-beam/beam/issues/337#issuecomment-575249344
insertNewUsers = runDebugInDb $
  let james     = User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
      betty     = User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
      sam       = User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"
  in (runInsert $ insert (_shoppingCartUsers shoppingCartDb) $ insertValues [ james, betty, sam ]) >>
     (runInsert $ insert (_shoppingCartUserAddresses shoppingCartDb) $ insertExpressions
       [ Address
         default_
         (val_ "123 Little Street")
         (val_ Nothing)
         (val_ "Boston")
         (val_ "MA")
         (val_ "12345")
         (val_ (pk james))
       , Address
         default_
         (val_ "222 Main Street")
         (val_ (Just "Ste 1"))
         (val_ "Houston")
         (val_ "TX")
         (val_ "8888")
         (val_ (pk betty))
       , Address
         default_
         (val_ "9999 Residence Ave")
         (val_ Nothing)
         (val_ "Sugarland")
         (val_ "TX")
         (val_ "8989")
         (val_ (pk betty)) ])

selectWithLens = runDebugInDb $
  do addresses <- runSelectReturningList $
       select $
       all_ $
       shoppingCartDb ^. shoppingCartUserAddresses
     mapM_ (liftIO . print) addresses

selectAllPairs = runDebugInDb $
  do allPairs <- runSelectReturningList $
       select $
       do user <- all_ (shoppingCartDb ^. shoppingCartUsers)
          address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
          guard_ (address ^. addressForUserId ==. user ^. userEmail)
          return (user, address)
     mapM_ (liftIO . print) allPairs

-- this version using `references_` rather than manual join
-- this so that we don't have to get the primary key of the referencing field
selectAllPairs2 = runDebugInDb $
  do allPairs <- runSelectReturningList $
       select $
       do user <- all_ (shoppingCartDb ^. shoppingCartUsers)
          address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
          guard_ (_addressForUser address `references_` user)
          return (user, address)
     mapM_ (liftIO . print) allPairs

-- this version using `related_` which acts like an `ON` clause
selectAllPairs3 = runDebugInDb $
  do allPairs <- runSelectReturningList $
       select $
       do address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
          user <- related_ (shoppingCartDb ^. shoppingCartUsers) (_addressForUser address)
          return (user, address)
     mapM_ (liftIO . print) allPairs

-- example to select using `WHERE` with a literal value
selectWhereLiteral = runDebugInDb $
  do bettysAddress <- runSelectReturningList $
       select $
       do address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
          guard_ (_addressForUser address ==. val_ (UserId "betty@example.com"))
          return address
     mapM_ (liftIO . print) bettysAddress

updateExample =
  do [james] <- runDebugInDb $
       do runUpdate $
            -- use the following if we have the whole record at hand and want
            -- to update every field
            -- save (shoppingCartDb ^. shoppingCartUsers) (james {_userPassword = "52a516..."})
            update
            (shoppingCartDb ^. shoppingCartUsers)
            (\user -> user ^. userPassword <-. val_ "52a516.....")
            (\user -> _userEmail user ==. val_ "james@example.com")
          runSelectReturningList $
            lookup_ (shoppingCartDb ^. shoppingCartUsers) (UserId "james@example.com")
     putStrLn ("James's new password is " ++ show (james ^. userPassword))

-- run an arbitrary query
-- example: `runQuery "SELECT * FROM addresses;"`
runQuery query =
  let command = ("sqlite3 " ++
                 dbFile ++
                 " '.header on' '.mode column' '" ++
                 unpack (replace "'" "\\'" $ pack query) ++
                 "' '.exit'")
  in do putStrLn command
        callCommand command

deleteExample = runDebugInDb $
  runDelete $
  delete
  (shoppingCartDb ^. shoppingCartUserAddresses)
  (\address -> address ^. addressCity ==. "Houston" &&.
               address ^. addressForUserId ==. "betty@example.com")

-- tutorial part 3
data ProductT f = Product
  { _productId          :: C f Int32
  , _productTitle       :: C f Text
  , _productDescription :: C f Text
  , _productPrice       :: C f Int32 }
  deriving (Generic, Beamable)

type Product = ProductT Identity
deriving instance Show Product

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (C f Int32) deriving (Generic, Beamable)
  primaryKey = ProductId . _productId

deriving instance Show (PrimaryKey AddressT Identity)

data OrderT f = Order
  { _orderId            :: C f Int32
  , _orderDate          :: C f LocalTime
  , _orderForUser       :: PrimaryKey UserT f
  , _orderShipToAddress :: PrimaryKey AddressT f
  , _orderShippingInfo  :: PrimaryKey ShippingInfoT (Nullable f) }
  deriving (Generic, Beamable)

type Order = OrderT Identity
deriving instance Show Order

instance Table OrderT where
  data PrimaryKey OrderT f = OrderId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = OrderId . _orderId

data ShippingCarrier = USPS | FedEx | UPS | DHL
  deriving (Show, Read, Eq, Ord, Enum)

data ShippingInfoT f = ShippingInfo
  { _shippingInfoId             :: C f Int32
  , _shippingInfoCarrier        :: C f ShippingCarrier
  , _shippingInfoTrackingNumber :: C f Text}
  deriving (Generic, Beamable)

type ShippingInfo = ShippingInfoT Identity
deriving instance Show ShippingInfo

instance Table ShippingInfoT where
  data PrimaryKey ShippingInfoT f = ShippingInfoId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = ShippingInfoId . _shippingInfoId

deriving instance Show (PrimaryKey ShippingInfoT (Nullable Identity))

deriving instance Show (PrimaryKey OrderT Identity)
deriving instance Show (PrimaryKey ProductT Identity)

data LineItemT f = LineItem
  { _lineItemInOrder    :: PrimaryKey OrderT f
  , _lineItemForProduct :: PrimaryKey ProductT f
  , _lineItemQuantity   :: C f Int32 }
  deriving (Generic, Beamable)

type LineItem = LineItemT Identity
deriving instance Show LineItem

instance Table LineItemT where
  data PrimaryKey LineItemT f = LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
    deriving (Generic, Beamable)
  primaryKey = LineItemId <$> _lineItemInOrder <*> _lineItemForProduct

data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers         :: f (TableEntity UserT)
  , _shoppingCartUserAddresses :: f (TableEntity AddressT)
  , _shoppingCartProducts      :: f (TableEntity ProductT)
  , _shoppingCartOrders        :: f (TableEntity OrderT)
  , _shoppingCartShippingInfos :: f (TableEntity ShippingInfoT)
  , _shoppingCartLineItems     :: f (TableEntity LineItemT) }
  deriving (Generic, Database be)

LineItem _ _ (LensFor lineItemQuantity) = tableLenses
Product
  (LensFor productId)
  (LensFor productTitle)
  (LensFor productDescription)
  (LensFor productPrice)
  = tableLenses

ShoppingCartDb
  (TableLens shoppingCartUsers)
  (TableLens shoppingCartUserAddresses)
  (TableLens shoppingCartProducts)
  (TableLens shoppingCartOrders)
  (TableLens shoppingCartShippingInfos)
  (TableLens shoppingCartLineItems)
  = dbLenses

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification` dbModification
  { _shoppingCartUserAddresses = setEntityName "addresses" <>
    modifyTableFields tableModification
    { _addressLine1   = "address1"
    , _addressLine2   = "address2"
    , _addressForUser = UserId "user" }
  , _shoppingCartProducts = setEntityName "products"
  , _shoppingCartOrders   = setEntityName "orders" <>
    modifyTableFields tableModification
    { _orderShippingInfo = ShippingInfoId "shipping_info__id" }
  , _shoppingCartShippingInfos = setEntityName "shipping_info" <>
    modifyTableFields tableModification
    { _shippingInfoId             = "id"
    , _shippingInfoCarrier        = "carrier"
    , _shippingInfoTrackingNumber = "tracking_number" }
  , _shoppingCartLineItems = setEntityName "line_items" }

-- create new tables for tutorial part 3

dbFile3 = "shoppingcart3.db"
getDb3 = open dbFile3

createTables3 =
  do conn <- getDb3
     execute_ conn "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
     execute_ conn "CREATE TABLE addresses ( id INTEGER PRIMARY KEY AUTOINCREMENT, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, user VARCHAR NOT NULL );"
     execute_ conn "CREATE TABLE products ( id INTEGER PRIMARY KEY AUTOINCREMENT, title VARCHAR NOT NULL, description VARCHAR NOT NULL, price INT NOT NULL );"
     execute_ conn "CREATE TABLE orders ( id INTEGER PRIMARY KEY AUTOINCREMENT, date TIMESTAMP NOT NULL, for_user__email VARCHAR NOT NULL, ship_to_address__id INT NOT NULL, shipping_info__id INT);"
     execute_ conn "CREATE TABLE shipping_info ( id INTEGER PRIMARY KEY AUTOINCREMENT, carrier VARCHAR NOT NULL, tracking_number VARCHAR NOT NULL);"
     execute_ conn "CREATE TABLE line_items (item_in_order__id INTEGER NOT NULL, item_for_product__id INTEGER NOT NULL, item_quantity INTEGER NOT NULL)"

users :: [User];
james, betty, sam :: User;
users@[james, betty, sam] =
  [ User "james@example.com" "James" "Smith"  "b4cc344d25a2efe540adbf2678e2304c" {- james -}
  , User "betty@example.com" "Betty" "Jones"  "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
  , User "sam@example.com"   "Sam"   "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

addresses :: [AddressT (QExpr Sqlite s)]
addresses =
  [ Address default_ (val_ "123 Little Street") (val_ Nothing) (val_ "Boston") (val_ "MA") (val_ "12345") (val_ (pk james))
  , Address default_ (val_ "222 Main Street") (val_ (Just "Ste 1")) (val_ "Houston") (val_ "TX") (val_ "8888") (val_ (pk betty))
  , Address default_ (val_ "9999 Residence Ave") (val_ Nothing) (val_ "Sugarland") (val_ "TX") (val_ "8989") (val_ (pk betty)) ]

products :: [ProductT (QExpr Sqlite s)]
products =
  [ Product default_ (val_ "Red Ball") (val_ "A bright red, very spherical ball") (val_ 1000)
  , Product default_ (val_ "Math Textbook") (val_ "Contains a lot of important math theorems and formulae") (val_ 2500)
  , Product default_ (val_ "Intro to Haskell") (val_ "Learn the best programming language in the world") (val_ 3000)
  , Product default_ (val_ "Suitcase") "A hard durable suitcase" 15000 ]

-- insert returning new elements (e.g., to get ID's)
-- test fixture
insertData3 =
 do conn <- getDb3
    (jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase) <- runBeamSqliteDebug putStrLn conn $
      do runInsert $
           insert (shoppingCartDb ^. shoppingCartUsers) $
           insertValues users
         [jamesAddress1, bettyAddress1, bettyAddress2] <- runInsertReturningList $
           insertReturning (shoppingCartDb ^. shoppingCartUserAddresses) $ insertExpressions addresses
         [redBall, mathTextbook, introToHaskell, suitcase] <- runInsertReturningList $
           insertReturning (shoppingCartDb ^. shoppingCartProducts) $ insertExpressions products
         pure ( jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase )
    putStrLn "hello"

-- marshalling a custom type
marshalExample =
  do conn <- getDb3
     bettyShippingInfo <- runBeamSqliteDebug putStrLn conn $
       do [bettyShippingInfo] <- runInsertReturningList $
            insertReturning (shoppingCartDb ^. shoppingCartShippingInfos) $
            insertExpressions
            [ ShippingInfo default_ (val_ USPS) (val_ "1234123134123123") ]
          pure bettyShippingInfo
     putStrLn ("Betty's shipping info is: " ++ (show bettyShippingInfo))

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite ShippingCarrier where
  fromBackendRow = read . unpack <$> fromBackendRow

-- insert transactions with timestamp using currentTimestamp_
-- won't show this here out of laziness
