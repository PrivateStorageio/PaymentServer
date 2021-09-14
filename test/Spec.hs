-- | Collect all of the various test groups into a single tree.

module Main
  ( main
  ) where

import Test.Tasty
  ( TestTree
  , testGroup
  , defaultMain
  )

import qualified Persistence
import qualified Metrics
import qualified Stripe
import qualified Redemption

tests :: TestTree
tests = testGroup "Tests"
  [ Persistence.tests
  , Metrics.tests
  , Stripe.tests
  , Redemption.tests
  ]

main = defaultMain tests
