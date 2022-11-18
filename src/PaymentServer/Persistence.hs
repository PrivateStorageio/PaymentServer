{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module PaymentServer.Persistence
  ( Voucher
  , Fingerprint
  , RedeemError(NotPaid, AlreadyRedeemed, DuplicateFingerprint, DatabaseUnavailable)
  , PaymentError(AlreadyPaid, PaymentFailed)
  , ProcessorResult
  , VoucherDatabase(payForVoucher, redeemVoucher, redeemVoucherWithCounter)
  , VoucherDatabaseState(MemoryDB, SQLiteDB)
  , memory
  , sqlite
  , upgradeSchema
  , latestVersion
  , readVersion
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

import qualified Prometheus as P

import Data.Maybe
  ( listToMaybe
  )

import Web.Stripe.Error
  ( StripeError
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
  | PaymentFailed StripeError
  deriving (Show)

instance Exception PaymentError

instance Eq PaymentError where
  AlreadyPaid == AlreadyPaid = True
  PaymentFailed self == PaymentFailed other = show self == show other
  _self == _other = False

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

-- | The result of completing payment processing.  This is either an error
-- indicating that the payment has *not* been completed (funds will not move)
-- or a payment processor-specific identifier for the completed transaction
-- (funds will move).
type ProcessorResult = Either PaymentError

-- | A VoucherDatabase provides persistence for state related to vouchers.
class VoucherDatabase d where
  -- | Change the state of the given voucher to indicate that it has been paid.
  payForVoucher
    :: d
    -- ^ The database in which to record the change
    -> Voucher
    -- ^ A voucher which should be considered paid
    -> IO (ProcessorResult a)
    -- ^ An operation which completes the payment.  This is evaluated in the
    -- context of a database transaction so that if it fails the voucher is
    -- not marked as paid in the database but if it succeeds the database
    -- state is not confused by a competing transaction run around the same
    -- time.
    -> IO (ProcessorResult a)
    -- ^ The result of the attempt to complete payment processing.

  -- | Attempt to redeem a voucher.  If it has not been redeemed before or it
  -- has been redeemed with the same fingerprint, the redemption succeeds.
  -- Otherwise, it fails.
  --
  -- This is a backwards compatibility API.  Callers should prefer
  -- redeemVoucherWithCounter.
  redeemVoucher
    :: d                            -- ^ The database
    -> Voucher                      -- ^ A voucher to consider for redemption
    -> Fingerprint                  -- ^ The retry-enabling fingerprint for this redemption
    -> IO (Either RedeemError Bool) -- ^ Left indicating the redemption is not allowed or Right indicating it is.
  redeemVoucher d v f = redeemVoucherWithCounter d v f 0

  -- | Attempt to redeem a voucher.  If it has not been redeemed before or it
  -- has been redeemed with the same counter and fingerprint, the redemption
  -- succeeds.  Otherwise, it fails.
  redeemVoucherWithCounter
    :: d                            -- ^ The database
    -> Voucher                      -- ^ A voucher to consider for redemption
    -> Fingerprint                  -- ^ The retry-enabling fingerprint for this redemption
    -> Integer                      -- ^ The counter for this redemption
    -> IO (Either RedeemError Bool) -- ^ Left indicating the redemption is not allowed or Right indicating it is.


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
  payForVoucher MemoryDB{ paid = paidRef } voucher pay = do
    -- Surely far from ideal...
    paid <- readIORef paidRef
    if Set.member voucher paid
      -- Avoid processing the payment if the voucher is already paid.
      then throwIO AlreadyPaid
      else
      do
        result <- pay
        case result of
          Right _ ->
            -- Only modify the paid set if the payment succeeds.
            modifyIORef paidRef (Set.insert voucher)

          Left _ -> return ()
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
      -- XXX things went poorly, should we handle with more detail?
      transformBusy panic = error $ "redeemVoucherHelper got bad input " <> show panic


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
  -> IO (Either RedeemError Bool)              -- ^ Right True for a new
                                               -- successful redemption, Right
                                               -- False for a retried
                                               -- successful redemption, left
                                               -- with details about why it
                                               -- failed.
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
          P.incCounter voucherRedeemed
          return $ Right True
        (True, Just fingerprint') ->
          if fingerprint == fingerprint' then
            return $ Right False
          else
            return $ Left AlreadyRedeemed


metricName :: Text -> Text
metricName name = mappend "payment_redemption_" name


voucherRedeemed :: P.Counter
voucherRedeemed
  = P.unsafeRegister
  $ P.counter
  $ P.Info (metricName "vouchers_redeemed")
  "The number of unique (voucher, counter) pairs which have been redeemed."


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
insertVoucher :: Sqlite.Connection -> Voucher -> IO (ProcessorResult a) -> IO (ProcessorResult a)
insertVoucher dbConn voucher pay =
  -- Begin an immediate transaction so that it includes the IO.  The
  -- transaction is immediate so that we can first check that the voucher is
  -- unique and then proceed to do the IO without worrying that another
  -- request will concurrently begin operating on the same voucher.
  Sqlite.withExclusiveTransaction dbConn $
  do
    -- Vouchers must be unique in this table.  Check to see if this one
    -- already exists.
    rows <- Sqlite.query dbConn "SELECT 1 FROM vouchers WHERE name = ?" (Sqlite.Only voucher) :: IO [Sqlite.Only Int]
    if length rows /= 0
      then throwIO AlreadyPaid
      else
      do
        -- If the voucher isn't present yet, try to finalize the payment.  If
        -- this succeeds, the transaction is committed and we expect the
        -- payment system to actually be moving money around.  If it fails, we
        -- expect the payment system *not* to move money around and the
        -- voucher should not be marked as paid.  The transaction will be
        -- rolled back so, indeed, it won't be marked thus.
        result <- pay
        case result of
          Right _ -> do
            Sqlite.execute dbConn "INSERT INTO vouchers (name, charge_id) VALUES (?, ?)" (voucher, Nothing :: Maybe Text)
            return result
          Left _err ->
            return result

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
      -- XXX handle any upgrade failures here!
      Sqlite.withExclusiveTransaction dbConn (upgradeSchema latestVersion dbConn)
      return dbConn

    connect :: IO Sqlite.Connection
    connect =
      bracketOnError (Sqlite.open . unpack $ path) Sqlite.close initialize
  in
    return . SQLiteDB $ connect


-- | updateVersions gives the SQL statements necessary to initialize the
-- database schema at each version that has ever existed.  The first element
-- is a list of SQL statements that modify an empty schema to create the first
-- version.  The second element is a list of SQL statements that modify the
-- first version to create the second version.  etc.
updateVersions :: [[Sqlite.Query]]
updateVersions =
  [ [ "CREATE TABLE vouchers (id INTEGER PRIMARY KEY, name TEXT UNIQUE)"
    , "CREATE TABLE redeemed (id INTEGER PRIMARY KEY, voucher_id INTEGER, counter INTEGER, fingerprint TEXT, FOREIGN KEY (voucher_id) REFERENCES vouchers(id))"
    ]
  , [ "CREATE TABLE version AS SELECT 2 AS version"
    -- Though we have this column it has become difficult to get the
    -- information from Stripe.  Since we don't have _much_ of a reason to
    -- keep it locally, we don't go to the trouble of getting it and so this
    -- column is no longer populated with a meaningful value.
    --
    -- If you want to know what charge id corresponds to what voucher, consult
    -- the database maintained by the payment processor.
    --
    -- We could probably drop this column now since we're not populating it
    -- with meaningful data but that's still more work and it's not really a
    -- burden to keep it so for now let's just leave it be.
    , "ALTER TABLE vouchers ADD COLUMN charge_id"
    ]
  ]

latestVersion :: Int
latestVersion = length updateVersions

-- | readVersion reads the schema version out of a database using the given
-- query function.  Since not all versions of the schema had an explicit
-- version marker, it digs around a little bit to find the answer.
readVersion :: Sqlite.Connection -> IO (Either UpgradeError Int)
readVersion conn = do
  versionExists <- doesTableExist "version"
  if versionExists
    -- If there is a version table then it knows the answer.
    then
    do
      versions <- Sqlite.query_ conn "SELECT version FROM version" :: IO [Sqlite.Only Int]
      case versions of
        [] -> return $ Left VersionMissing
        (Sqlite.Only v):[] -> return $ Right v
        vs -> return $ Left $ ExcessVersions (map Sqlite.fromOnly vs)
    else
    do
      vouchersExists <- doesTableExist "vouchers"
      if vouchersExists
        -- If there is a vouchers table then we have version 1
        then return $ Right 1
        -- Otherwise we have version 0
        else return $ Right 0

  where
    doesTableExist :: Text -> IO Bool
    doesTableExist name = do
      (Sqlite.Only count):[] <-
        Sqlite.query
        conn
        "SELECT COUNT(*) FROM [sqlite_master] WHERE [type] = 'table' AND [name] = ?"
        (Sqlite.Only name) :: IO [Sqlite.Only Int]
      return $ count > 0



-- | upgradeSchema determines what schema changes need to be applied to the
-- database associated with a connection to make the schema match the
-- requested version.
upgradeSchema :: Int -> Sqlite.Connection -> IO (Either UpgradeError ())
upgradeSchema targetVersion conn = do
  errOrCurrentVersion <- readVersion conn
  case errOrCurrentVersion of
    Left err -> return $ Left err
    Right currentVersion -> perhapsUpgrade targetVersion currentVersion

  where
    perhapsUpgrade :: Int -> Int -> IO (Either UpgradeError ())
    perhapsUpgrade targetVersion currentVersion =
      case compareVersion targetVersion currentVersion of
        Lesser -> return $ Left DatabaseSchemaTooNew
        Equal -> return $ Right ()
        Greater -> runUpgrades currentVersion targetVersion

    runUpgrades :: Int -> Int -> IO (Either UpgradeError ())
    runUpgrades currentVersion targetVersion =
      let
        upgrades :: [[Sqlite.Query]]
        upgrades = drop currentVersion $ take targetVersion updateVersions

        oneStep :: [Sqlite.Query] -> IO [()]
        oneStep = mapM $ Sqlite.execute_ conn
      in do
        mapM_ oneStep upgrades
        return $ Right ()


data UpgradeError
  = VersionMissing
  | ExcessVersions [Int]
  | DatabaseSchemaTooNew
  deriving (Show, Eq)

data ComparisonResult = Lesser | Equal | Greater

compareVersion :: Int -> Int -> ComparisonResult
compareVersion a b
  | a < b     = Lesser
  | a == b    = Equal
  | otherwise = Greater
