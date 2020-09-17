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

tests :: TestTree
tests = testGroup "Tests"
  [ Persistence.tests
  , Metrics.tests
  ]

main = defaultMain tests
