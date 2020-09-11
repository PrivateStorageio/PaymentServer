{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module PaymentServer.Persistence
  ( Voucher
  , Fingerprint
  , RedeemError(NotPaid, AlreadyRedeemed, DuplicateFingerprint, DatabaseUnavailable)
  , PaymentError(AlreadyPaid, PaymentFailed)
  , VoucherDatabase(payForVoucher, redeemVoucher, redeemVoucherWithCounter)
  , VoucherDatabaseState(MemoryDB, SQLiteDB)
  , memory
  , sqlite
  ) where

import Control.Exception
  ( Exception
  , throwIO
  , catch
  , bracket
  , bracketOnError
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
  -- | The payment transaction has failed.
  | PaymentFailed
  deriving (Show, Eq)

instance Exception PaymentError

-- | Reasons that a voucher cannot be redeemed.
data RedeemError =
  -- | The voucher has not been paid for.
  NotPaid
  -- | The voucher has already been redeemed.
  | AlreadyRedeemed
  -- | The fingerprint given has already been seen.  Redemption with a
  -- duplicate fingerprint is disallowed.  Even though tokens could be issued
  -- in this case, they would be the same as tokens already issued for a
  -- different redemption attempt.  The re-issued tokens are not distinct from
  -- the originals and attempts to spend them will lead to double-spend
  -- errors.  A well-behaved client will never request tokens with a duplicate
  -- fingerprint.  We check for this case to prevent a misbehaving client from
  -- accidentally creating worthless tokens.
  | DuplicateFingerprint
  -- | The database is too busy right now.  Try again later.
  | DatabaseUnavailable
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

-- | A RedemptionKey is a unique key that identifies an attempt to redeem a
-- voucher for some tokens.  It includes a counter value distinct from the
-- voucher value to allow one voucher to be redeemed for more than one batch
-- of tokens.  This allows partial progress on redemption when a voucher is
-- worth many, many tokens.  Redemption is restricted to a single successful
-- attempt per RedemptionKey (with retries using the same Fingerprint
-- allowed).
type RedemptionKey = (Voucher, Integer)

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
  --
  -- This is a backwards compatibility API.  Callers should prefer
  -- redeemVoucherWithCounter.
  redeemVoucher
    :: d                          -- ^ The database
    -> Voucher                    -- ^ A voucher to consider for redemption
    -> Fingerprint                -- ^ The retry-enabling fingerprint for this redemption
    -> IO (Either RedeemError ()) -- ^ Left indicating the redemption is not allowed or Right indicating it is.
  redeemVoucher d v f = redeemVoucherWithCounter d v f 0

  -- | Attempt to redeem a voucher.  If it has not been redeemed before or it
  -- has been redeemed with the same counter and fingerprint, the redemption
  -- succeeds.  Otherwise, it fails.
  redeemVoucherWithCounter
    :: d                          -- ^ The database
    -> Voucher                    -- ^ A voucher to consider for redemption
    -> Fingerprint                -- ^ The retry-enabling fingerprint for this redemption
    -> Integer                    -- ^ The counter for this redemption
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
    -- | A mapping from redeemed (voucher, counter) pairs to fingerprints
    -- associated with the redemption.
  , redeemed :: IORef (Map.Map RedemptionKey Fingerprint)
    -- | A mapping from fingerprints to redemption details for successful
    -- redemptions.  This is the logical reverse of `redeemed` and should
    -- always contain the same values as `redeemed`, but reversed.  It is
    -- maintained separately for efficient lookup by fingerprint.
  , fingerprints :: IORef (Map.Map Fingerprint RedemptionKey)
  }
  | SQLiteDB { connect :: IO Sqlite.Connection }

instance VoucherDatabase VoucherDatabaseState where
  payForVoucher MemoryDB{ paid = paidRef, redeemed = redeemed } voucher pay = do
    -- Surely far from ideal...
    paid <- readIORef paidRef
    if Set.member voucher paid
      -- Avoid processing the payment if the voucher is already paid.
      then throwIO AlreadyPaid
      else
      do
        result <- pay
        -- Only modify the paid set if the payment succeeds.
        modifyIORef paidRef (Set.insert voucher)
        return result

  payForVoucher SQLiteDB{ connect = connect } voucher pay =
    bracket connect Sqlite.close $ \conn ->
    insertVoucher conn voucher pay

  redeemVoucherWithCounter MemoryDB{ paid, redeemed, fingerprints } voucher fingerprint counter =
    let
      isVoucherPaid paid voucher = Set.member voucher <$> readIORef paid
      lookupFingerprint redeemed key = Map.lookup key <$> readIORef redeemed
      lookupVoucherCounter fingerprints fingerprint =
        Map.lookup fingerprint <$> readIORef fingerprints
      markVoucherRedeemed redeemed fingerprints key fingerprint = do
        modifyIORef redeemed $ Map.insert key fingerprint
        modifyIORef fingerprints $ Map.insert fingerprint key
    in
      redeemVoucherHelper
      (isVoucherPaid paid)
      (lookupFingerprint redeemed)
      (lookupVoucherCounter fingerprints)
      (markVoucherRedeemed redeemed fingerprints)
      voucher
      counter
      fingerprint

  redeemVoucherWithCounter SQLiteDB { connect = connect } voucher fingerprint counter =
    bracket connect Sqlite.close redeemIt `catch` transformBusy
    where
      redeemIt conn =
        Sqlite.withExclusiveTransaction conn $
        redeemVoucherHelper
        (isVoucherPaid conn)
        (getVoucherFingerprint conn)
        (getVoucherCounterForFingerprint conn)
        (insertVoucherAndFingerprint conn)
        voucher
        counter
        fingerprint

      transformBusy (Sqlite.SQLError Sqlite.ErrorBusy _ _) =
        return . Left $ DatabaseUnavailable


-- | Look up the voucher, counter tuple which previously performed a
-- redemption using the given fingerprint.
getVoucherCounterForFingerprint :: Sqlite.Connection -> Fingerprint -> IO (Maybe RedemptionKey)
getVoucherCounterForFingerprint dbConn fingerprint =
  let
    sql = "SELECT vouchers.name, redeemed.counter \
          \FROM vouchers \
          \INNER JOIN redeemed \
          \ON vouchers.id = redeemed.voucher_id \
          \AND redeemed.fingerprint = ?"
  in
    listToMaybe <$> Sqlite.query dbConn sql (Sqlite.Only fingerprint)


-- | Allow a voucher to be redeemed if it has been paid for and not redeemed
-- before or redeemed with the same fingerprint.
redeemVoucherHelper
  :: (Voucher -> IO Bool)                      -- ^ Has the given voucher been
                                               -- paid for?
  -> (RedemptionKey -> IO (Maybe Fingerprint)) -- ^ If it has been redeemed,
                                               -- with what fingerprint?
  -> (Fingerprint -> IO (Maybe RedemptionKey)) -- ^ What redemption attempt
                                               -- has the given fingerprint
                                               -- been used with before, if
                                               -- any?
  -> (RedemptionKey -> Fingerprint -> IO ())   -- ^ Mark the redemption as
                                               -- successful.
  -> Voucher                                   -- ^ The voucher being used in
                                               -- this attempt.
  -> Integer                                   -- ^ The counter being used in
                                               -- this account.
  -> Fingerprint                               -- ^ The fingerprint of the
                                               -- this attempt.
  -> IO (Either RedeemError ())                -- ^ Right for successful
                                               -- redemption, left with
                                               -- details about why it failed.
redeemVoucherHelper isVoucherPaid lookupFingerprint lookupVoucherCounter markVoucherRedeemed voucher counter fingerprint = do
  paid <- isVoucherPaid voucher
  priorUse <- lookupVoucherCounter fingerprint
  if (priorUse /= Just (voucher, counter)) && (priorUse /= Nothing)
    then return $ Left DuplicateFingerprint
    else
    do
      existingFingerprint <- lookupFingerprint (voucher, counter)
      case (paid, existingFingerprint) of
        (False, _) ->
          return $ Left NotPaid
        (True, Nothing) -> do
          markVoucherRedeemed (voucher, counter) fingerprint
          return $ Right ()
        (True, Just fingerprint') ->
          if fingerprint == fingerprint' then
            return $ Right ()
          else
            return $ Left AlreadyRedeemed


-- | Create a new, empty MemoryVoucherDatabase.
memory :: IO VoucherDatabaseState
memory = do
  paid <- newIORef mempty
  redeemed <- newIORef mempty
  fingerprints <- newIORef mempty
  return $ MemoryDB paid redeemed fingerprints

instance FromRow Fingerprint where
  fromRow = Sqlite.field

-- | Checks if the given `voucher` is paid.
isVoucherPaid :: Sqlite.Connection -> Voucher -> IO Bool
isVoucherPaid dbConn voucher =
  not . null <$> (Sqlite.query dbConn "SELECT 1 FROM vouchers WHERE vouchers.name = ? LIMIT 1" (Sqlite.Only voucher) :: IO [Sqlite.Only Int])

-- | Retrieve an existing redemption fingerprint for the given voucher and
-- counter, if there is one.
getVoucherFingerprint :: Sqlite.Connection -> RedemptionKey -> IO (Maybe Fingerprint)
getVoucherFingerprint dbConn (voucher, counter) =
  let
    sql = "SELECT redeemed.fingerprint \
          \FROM vouchers \
          \INNER JOIN redeemed \
          \ON vouchers.id = redeemed.voucher_id \
          \AND vouchers.name = ? \
          \AND redeemed.counter = ?"
  in
    listToMaybe <$> Sqlite.query dbConn sql ((voucher :: Text), (counter :: Integer))

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
insertVoucherAndFingerprint :: Sqlite.Connection -> RedemptionKey -> Fingerprint -> IO ()
insertVoucherAndFingerprint dbConn (voucher, counter) fingerprint =
  let
    sql = "INSERT INTO redeemed (voucher_id, counter, fingerprint) \
          \VALUES ((SELECT id FROM vouchers WHERE name = ?), ?, ?)"
  in
    Sqlite.execute dbConn sql (voucher, counter, fingerprint)

-- | Open and create (if necessary) a SQLite3 database which can persistently
-- store all of the relevant information about voucher state.
sqlite :: Text -> IO VoucherDatabaseState
sqlite path =
  let
    initialize :: Sqlite.Connection -> IO Sqlite.Connection
    initialize dbConn = do
      let exec = Sqlite.execute_ dbConn
      exec "PRAGMA busy_timeout = 60000"
      exec "PRAGMA foreign_keys = ON"
      Sqlite.withExclusiveTransaction dbConn $ do
        exec "CREATE TABLE IF NOT EXISTS vouchers (id INTEGER PRIMARY KEY, name TEXT UNIQUE)"
        exec "CREATE TABLE IF NOT EXISTS redeemed (id INTEGER PRIMARY KEY, voucher_id INTEGER, counter INTEGER, fingerprint TEXT, FOREIGN KEY (voucher_id) REFERENCES vouchers(id))"
      return dbConn

    connect :: IO Sqlite.Connection
    connect =
      bracketOnError (Sqlite.open . unpack $ path) Sqlite.close initialize
  in
    return . SQLiteDB $ connect
