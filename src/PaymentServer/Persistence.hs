{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PaymentServer.Persistence
  ( Voucher
  , Fingerprint
  , RedeemError(NotPaid, AlreadyRedeemed)
  , VoucherDatabase(payForVoucher, redeemVoucher)
  , VoucherDatabaseState(MemoryDB, SQLiteDB)
  , memory
  , getDBConnection
  ) where

import Data.Text
  ( Text
  , unpack
  )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IORef
  ( IORef
  , newIORef
  , modifyIORef
  , readIORef
  )
import qualified Database.SQLite.Simple as Sqlite
import           Database.SQLite.Simple.FromRow
  ( FromRow(fromRow)
  )
import Data.Maybe
  ( listToMaybe
  )

-- | A voucher is a unique identifier which can be associated with a payment.
-- A paid voucher can be redeemed for ZKAPs which can themselves be exchanged
-- for service elsewhere with better privacy-preserving properties than the
-- voucher itself.
type Voucher = Text

-- | Reasons that a voucher cannot be redeemed.
data RedeemError =
  -- | The voucher has not been paid for.
  NotPaid
  -- | The voucher has already been redeemed.
  | AlreadyRedeemed
  deriving (Show, Eq)

-- | A fingerprint cryptographically identifies a redemption of a voucher.
-- When a voucher is redeemed, a number of random tokens are received
-- alongside it.  These tokens are signed to create ZKAPs to return to the
-- redeemer.  To support fault tolerance (particularly network fault
-- tolerance) it is allowed to redeem a voucher more than once *so long as*
-- the same tokens are received with each attempt.  The tokens are
-- cryptographically hashed to produce a fingerprint that can be persisted
-- along with the voucher state and checked on possibly-duplicate redemption
-- to support this case.
type Fingerprint = Text

-- | A VoucherDatabase provides persistence for state related to vouchers.
class VoucherDatabase d where
  -- | Change the state of the given voucher to indicate that it has been paid.
  payForVoucher
    :: d             -- ^ The database in which to record the change
    -> Voucher       -- ^ A voucher which should be considered paid
    -> IO ()

  -- | Attempt to redeem a voucher.  If it has not been redeemed before or it
  -- has been redeemed with the same fingerprint, the redemption succeeds.
  -- Otherwise, it fails.
  redeemVoucher
    :: d                          -- ^ The database
    -> Voucher                    -- ^ A voucher to consider for redemption
    -> Fingerprint                -- ^ The retry-enabling fingerprint for this redemption
    -> IO (Either RedeemError ()) -- ^ Left indicating the redemption is not allowed or Right indicating it is.

-- | VoucherDatabaseState is a type that captures whether we are using an
-- in-memory voucher database that only persists state in-memory or
-- a persistent one that writes to SQLite database.  The in-memory database
-- state does not outlive the process which creates it (nor even the
-- VoucherDatabase value). This is primarily useful for testing.
-- `SQLiteDB` is useful for the production use where the state needs to persist.
data VoucherDatabaseState =
  MemoryDB {
    -- | A set of vouchers which have been paid for.
    paid :: IORef (Set.Set Voucher)
    -- | A mapping from redeemed vouchers to fingerprints associated with the
    -- redemption.
  , redeemed :: IORef (Map.Map Voucher Fingerprint)
  }
  | SQLiteDB { conn :: Sqlite.Connection }

instance VoucherDatabase VoucherDatabaseState where
  payForVoucher MemoryDB{ paid = paid, redeemed = redeemed } voucher =
    modifyIORef paid (Set.insert voucher)

  payForVoucher SQLiteDB{ conn = conn } voucher =
    insertVoucher conn voucher

  redeemVoucher MemoryDB{ paid = paid, redeemed = redeemed } voucher fingerprint = do
    unpaid <- Set.notMember voucher <$> readIORef paid
    existingFingerprint <- Map.lookup voucher <$> readIORef redeemed
    let insertFn voucher fingerprint = modifyIORef redeemed (Map.insert voucher fingerprint)
    redeemVoucherHelper (unpaid, existingFingerprint) voucher fingerprint insertFn

  redeemVoucher SQLiteDB { conn = conn } voucher fingerprint = do
    unpaid <- isVoucherUnpaid conn voucher
    existingFingerprint <- listToMaybe <$> getVoucherFingerprint conn voucher
    let insertFn = insertVoucherAndFingerprint conn
    redeemVoucherHelper (unpaid, existingFingerprint) voucher fingerprint insertFn

redeemVoucherHelper :: (Bool, Maybe Fingerprint) -> Voucher -> Fingerprint -> (Voucher -> Fingerprint -> IO ()) -> IO (Either RedeemError ())
redeemVoucherHelper (unpaid, existingFingerprint) voucher fingerprint insertFn = do
  case (unpaid, existingFingerprint) of
    (True, _) ->
      return $ Left NotPaid
    (False, Nothing) -> do
      insertFn voucher fingerprint
      return $ Right ()
    (False, Just fingerprint') ->
      if fingerprint == fingerprint' then
        return $ Right ()
      else
        return $ Left AlreadyRedeemed

-- | Create a new, empty MemoryVoucherDatabase.
memory :: IO VoucherDatabaseState
memory = do
  paid <- newIORef mempty
  redeemed <- newIORef mempty
  return $ MemoryDB paid redeemed

instance FromRow Fingerprint where
  fromRow = Sqlite.field

-- | Checks if the given `voucher` is unpaid.
isVoucherUnpaid :: Sqlite.Connection -> Voucher -> IO Bool
isVoucherUnpaid dbConn voucher =
  null <$> (Sqlite.query dbConn "SELECT DISTINCT name FROM vouchers INNER JOIN redeemed WHERE vouchers.id != redeemed.voucher_id AND vouchers.name = ?" (Sqlite.Only voucher) :: IO [Voucher])

getVoucherFingerprint :: Sqlite.Connection -> Voucher -> IO [Fingerprint]
getVoucherFingerprint dbConn voucher =
  Sqlite.query dbConn "SELECT redeemed.fingerprint FROM vouchers INNER JOIN redeemed ON vouchers.id = redeemed.voucher_id AND vouchers.name = ?" (Sqlite.Only voucher)

insertVoucher :: Sqlite.Connection -> Voucher -> IO ()
insertVoucher dbConn voucher =
  Sqlite.execute dbConn "INSERT INTO vouchers (name) VALUES (?)" (Sqlite.Only voucher)

insertVoucherAndFingerprint :: Sqlite.Connection -> Voucher -> Fingerprint -> IO ()
insertVoucherAndFingerprint dbConn voucher fingerprint =
  Sqlite.execute dbConn "INSERT INTO redeemed (voucher_id, fingerprint) VALUES ((SELECT id FROM vouchers_new WHERE name = ?), ?)" (voucher, fingerprint)

-- | create and open a database with a given `name` and create the `voucher` table
-- and `redeemed` table with the provided schema
getDBConnection :: Text -> IO VoucherDatabaseState
getDBConnection name = do
  dbConn <- Sqlite.open (unpack name)
  Sqlite.execute_ dbConn "CREATE TABLE IF NOT EXISTS vouchers (id INTEGER PRIMARY KEY, name TEXT UNIQUE)"
  Sqlite.execute_ dbConn "CREATE TABLE IF NOT EXISTS redeemed (id INTEGER PRIMARY KEY, voucher_id INTEGER, fingerprint TEXT, FOREIGN KEY (voucher_id) REFERENCES vouchers(id))"
  return $ SQLiteDB dbConn
