{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Monadic
import Snowflake
import Data.Int (Int64)

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
-- ascending order

-- prop_asc TODO

--------------------------------------------------------------------------
-- no-repeat

-- prop_np TODO

--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll
