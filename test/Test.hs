{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Monadic
import Snowflake
import Data.Int (Int64)
import Data.List (sort)

--------------------------------------------------------------------------
-- constant

maxScale = 10^6

--------------------------------------------------------------------------
-- arbitrary instance

instance Arbitrary IdWorker where
  arbitrary = do
    sequence     <- choose (0, 2^(_sequenceBits defaultConf) - 1) :: Gen Int64
    workerId     <- choose (0, 2^(_workerIdBits defaultConf) - 1) :: Gen Int64
    datacenterId <- choose (0, 2^(_datacenterIdBits defaultConf) - 1) :: Gen Int64
    return IdWorker { _sequence = sequence
                    , _workerId = workerId
                    , _datacenterId = datacenterId
                    , _timestamp = 0
                    , _conf = defaultConf
                    }

--------------------------------------------------------------------------
-- size consist with n

prop_sc worker n = 0 <= n && n <= maxScale  ==> monadicIO $ do
  (ids, _) <- run $ nexts worker n
  assert $ length ids == n

--------------------------------------------------------------------------
-- strict ascending order

isStrictAsc :: Ord a => [a] -> Bool
isStrictAsc (x:y:xs)
  | x < y = isStrictAsc (y:xs)
  | otherwise = False
isStrictAsc _ = True

prop_asc worker n = 0 <= n && n <= maxScale  ==> monadicIO $ do
  (ids, _) <- run $ nexts worker n
  assert $ isStrictAsc ids

--------------------------------------------------------------------------
-- no-repeat

isNoRepeat :: Ord a => [a] -> Bool
isNoRepeat = isNoRepeat' . sort

isNoRepeat' :: Eq a => [a] -> Bool
isNoRepeat' (x:y:xs)
  | x /= y = isNoRepeat' (y:xs)
  | otherwise = False
isNoRepeat' _ = True

prop_np worker n = 0 <= n && n <= maxScale ==> monadicIO $ do
  (ids, _) <- run $ nexts worker n
  assert $ isNoRepeat ids

--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll
