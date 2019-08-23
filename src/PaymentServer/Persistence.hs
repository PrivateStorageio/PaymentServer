module PaymentServer.Persistence
  ( Voucher
  , VoucherDatabase(payForVoucher, redeemVoucher)
  , memory
  ) where

import Data.Text
  ( Text
  )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IORef
  ( IORef
  , newIORef
  , modifyIORef
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

-- | MemoryVoucherDatabase is a voucher database that only persists state
-- in-memory.  The state does not outlive the process which creates it.  This
-- is primarily useful for testing.
data MemoryVoucherDatabase =
  Memory
  { paid :: IORef (Set.Set Voucher)
  , redeemed :: IORef (Map.Map Voucher Fingerprint)
  }

instance VoucherDatabase MemoryVoucherDatabase where
  payForVoucher Memory{ paid = paid, redeemed = redeemed } voucher = do
    modifyIORef paid (Set.insert voucher)
    return ()

-- | Create a new, empty MemoryVoucherDatabase.
memory :: IO MemoryVoucherDatabase
memory = do
  paid <- newIORef mempty
  redeemed <- newIORef mempty
  return $ Memory paid redeemed