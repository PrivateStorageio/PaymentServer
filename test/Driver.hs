-- XXX
-- Generated with
--    $ rm test/Driver.hs && stack exec tasty-discover "./test/Driver.hs" . ./test/Driver.hs
-- Need to automate that somehow.
{-# LINE 1 "./test/Driver.hs" #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main, ingredients, tests) where
import Prelude
import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T
import qualified Test.Tasty.QuickCheck as QC

import qualified Test.Tasty.Hspec as HS

import qualified SpecPersistence

import qualified SpecStripe

import qualified SpecRedemption



tests :: IO T.TestTree
tests = do
  t0 <- HS.testSpec "memory" SpecPersistence.spec_memory

  t1 <- HS.testSpec "webhook" SpecStripe.spec_webhook

  t2 <- pure $ QC.testProperty "getVoucherFindsVoucher" SpecStripe.prop_getVoucherFindsVoucher

  t3 <- pure $ QC.testProperty "getVoucherWithoutVoucher" SpecStripe.prop_getVoucherWithoutVoucher

  t4 <- HS.testSpec "simple" SpecRedemption.spec_simple

  t5 <- HS.testSpec "memory db" SpecRedemption.spec_memory_db

  pure $ T.testGroup "./test/Driver.hs" [t0,t1,t2,t3,t4,t5]
ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients
main :: IO ()
main = do
  args <- E.getArgs
  E.withArgs ([] ++ args) $    tests >>= T.defaultMainWithIngredients ingredients
