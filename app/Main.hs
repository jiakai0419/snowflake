module Main where

import Snowflake

worker1 = IdWorker { _sequence = 0
                   , _workerId = 7
                   , _datacenterId = 23
                   , _timestamp = 0
                   , _conf = defaultConf
                   }

main :: IO ()
main = do
  (ids, newWorker) <- nexts worker1 1000000
  print ids
  print newWorker
