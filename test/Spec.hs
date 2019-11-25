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

tests :: TestTree
tests = testGroup "Tests"
  [ Persistence.tests
  ]

main = defaultMain tests
