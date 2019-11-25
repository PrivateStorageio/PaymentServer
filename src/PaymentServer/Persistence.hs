{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PaymentServer.Persistence
  ( Voucher
  , Fingerprint
  , RedeemError(NotPaid, AlreadyRedeemed)
  , PaymentError(AlreadyPaid)
  , VoucherDatabase(payForVoucher, redeemVoucher)
  , VoucherDatabaseState(MemoryDB, SQLiteDB)
  , memory
  , getDBConnection
  ) where

import Control.Exception
  ( Exception
  , throwIO
  , catch
  )

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

-- | Reasons that a voucher cannot be paid for.
data PaymentError =
  -- | The voucher has already been paid for.
  AlreadyPaid
  deriving (Show, Eq)

instance Exception PaymentError

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
    -> IO a          -- ^ An operation which completes the payment.  This is
                     -- evaluated in the context of a database transaction so
                     -- that if it fails the voucher is not marked as paid in
                     -- the database but if it succeeds the database state is
                     -- not confused by a competing transaction run around the
                     -- same time.
    -> IO a

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
  payForVoucher MemoryDB{ paid = paid, redeemed = redeemed } voucher pay = do
    result <- pay
    modifyIORef paid (Set.insert voucher)
    return result

  payForVoucher SQLiteDB{ conn = conn } voucher pay =
    insertVoucher conn voucher pay

  redeemVoucher MemoryDB{ paid = paid, redeemed = redeemed } voucher fingerprint = do
    unpaid <- Set.notMember voucher <$> readIORef paid
    existingFingerprint <- Map.lookup voucher <$> readIORef redeemed
    let insertFn = (modifyIORef redeemed .) . Map.insert
    redeemVoucherHelper unpaid existingFingerprint voucher fingerprint insertFn

  redeemVoucher SQLiteDB { conn = conn } voucher fingerprint = Sqlite.withExclusiveTransaction conn $ do
    unpaid <- isVoucherUnpaid conn voucher
    existingFingerprint <- getVoucherFingerprint conn voucher
    let insertFn = insertVoucherAndFingerprint conn
    redeemVoucherHelper unpaid existingFingerprint voucher fingerprint insertFn

-- | Allow a voucher to be redeemed if it has been paid for and not redeemed
-- before or redeemed with the same fingerprint.
redeemVoucherHelper
  :: Bool                                -- ^ Has the voucher been paid for?
  -> Maybe Fingerprint                   -- ^ If it has been redeemed before,
                                         -- with what fingerprint?
  -> Voucher                             -- ^ The voucher in question.
  -> Fingerprint                         -- ^ The fingerprint associated with
                                         -- the new redemption attempt.
  -> (Voucher -> Fingerprint -> IO ())   -- ^ A function to mark the voucher
                                         -- as redeemed if this redemption
                                         -- should succeed.
  -> IO (Either RedeemError ())          -- ^ Right for successful redemption,
                                         -- left with details about why it
                                         -- failed.
redeemVoucherHelper unpaid existingFingerprint voucher fingerprint insertFn =
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
  null <$> (Sqlite.query dbConn "SELECT 1 FROM vouchers WHERE vouchers.name = ? LIMIT 1" (Sqlite.Only voucher) :: IO [Sqlite.Only Int])

-- | Retrieve an existing redemption fingerprint for the given voucher, if
-- there is one.
getVoucherFingerprint :: Sqlite.Connection -> Voucher -> IO (Maybe Fingerprint)
getVoucherFingerprint dbConn voucher =
  listToMaybe <$> Sqlite.query dbConn "SELECT redeemed.fingerprint FROM vouchers INNER JOIN redeemed ON vouchers.id = redeemed.voucher_id AND vouchers.name = ?" (Sqlite.Only voucher)

-- | Mark the given voucher as paid in the database.
insertVoucher :: Sqlite.Connection -> Voucher -> IO a -> IO a
insertVoucher dbConn voucher pay =
  -- Begin an immediate transaction so that it includes the IO.  The first
  -- thing we do is execute our one and only statement so the transaction is
  -- immediate anyway but it doesn't hurt to be explicit.
  Sqlite.withImmediateTransaction dbConn $
  do
    -- Vouchers must be unique in this table.  This might fail if someone is
    -- trying to double-pay for a voucher.  In this case, we won't ever
    -- finalize the payment.
    Sqlite.execute dbConn "INSERT INTO vouchers (name) VALUES (?)" (Sqlite.Only voucher)
      `catch` handleConstraintError
    -- If we managed to insert the voucher, try to finalize the payment.  If
    -- this succeeds, the transaction is committed and we expect the payment
    -- system to actually be moving money around.  If it fails, we expect the
    -- payment system *not* to move money around and the voucher should not be
    -- marked as paid.  The transaction will be rolled back so, indeed, it
    -- won't be marked thus.
    pay

  where
    handleConstraintError Sqlite.SQLError { Sqlite.sqlError = Sqlite.ErrorConstraint } =
      throwIO AlreadyPaid
    handleConstraintError e =
      throwIO e


-- | Mark the given voucher as having been redeemed (with the given
-- fingerprint) in the database.
insertVoucherAndFingerprint :: Sqlite.Connection -> Voucher -> Fingerprint -> IO ()
insertVoucherAndFingerprint dbConn voucher fingerprint =
  Sqlite.execute dbConn "INSERT INTO redeemed (voucher_id, fingerprint) VALUES ((SELECT id FROM vouchers WHERE name = ?), ?)" (voucher, fingerprint)

-- | Create and open a database with a given `name` and create the `voucher`
-- table and `redeemed` table with the provided schema.
getDBConnection :: Text -> IO VoucherDatabaseState
getDBConnection path = do
  dbConn <- Sqlite.open (unpack path)
  Sqlite.execute_ dbConn "PRAGMA foreign_keys = ON"
  Sqlite.execute_ dbConn "CREATE TABLE IF NOT EXISTS vouchers (id INTEGER PRIMARY KEY, name TEXT UNIQUE)"
  Sqlite.execute_ dbConn "CREATE TABLE IF NOT EXISTS redeemed (id INTEGER PRIMARY KEY, voucher_id INTEGER, fingerprint TEXT, FOREIGN KEY (voucher_id) REFERENCES vouchers(id))"
  return $ SQLiteDB dbConn
