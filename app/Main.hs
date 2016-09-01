module Main where

import Snowflake
import Data.Time.Exts.Unix

worker1 = IdWorker { _sequence = 0
                   , _workerId = 7
                   , _datacenterId = 23
                   , _lastTimestamp = 0
                   , _conf = defaultConf
                   }

main :: IO ()
main = print $ next worker1 1472752884000
