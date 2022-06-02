module Main where

import Data.Text
  ( pack
  )

import Web.Stripe.Types
  ( ChargeId(ChargeId)
  )

import System.Environment
  ( getArgs
  )
import PaymentServer.Persistence
  ( VoucherDatabase(payForVoucher)
  , sqlite
  )

main :: IO ()
main = do
  [db_path, voucher, chargeId] <- getArgs
  db <- sqlite $ pack db_path
  payForVoucher db (pack voucher) (return . Right . ChargeId . pack $ chargeId)
  return ()
