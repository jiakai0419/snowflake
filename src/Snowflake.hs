{-# LANGUAGE TemplateHaskell #-}
module Snowflake
  ( Timestamp
  , Conf(..)
  , IdWorker(..)
  , defaultConf
  , next
  ) where

import Prelude hiding (sequence)
import Data.Int (Int64)
import Data.Maybe (Maybe)
import Data.Bits ( (.|.)
                 , shiftL
                 )
import Lens.Micro hiding (element)
import Lens.Micro.TH
import Lens.Micro.Extras

type Timestamp = Int64

data Conf = Conf { _sequenceBits :: Int
                 , _workerIdBits :: Int
                 , _datacenterIdBits :: Int
                 , _timestampBase :: Timestamp
                 } deriving (Show)

data IdWorker = IdWorker { _sequence :: Int64
                         , _workerId :: Int64
                         , _datacenterId :: Int64
                         , _lastTimestamp :: Timestamp
                         , _conf :: Conf
                         } deriving (Show)

makeLenses ''Conf
makeLenses ''IdWorker

defaultConf = Conf { _sequenceBits = 12
                   , _workerIdBits = 5
                   , _datacenterIdBits = 5
                   , _timestampBase = 1472733628921
                   }

next :: IdWorker -> Timestamp -> (Maybe Int64, IdWorker)
next worker timestamp =
  if sequenceIsFull worker timestamp
     then (Nothing, worker)
     else (Just $ next' worker timestamp, worker {_sequence = newSequence worker timestamp, _lastTimestamp = timestamp})

next' :: IdWorker -> Timestamp -> Int64
next' worker timestamp = tBit .|. dBit .|. wBit .|. sBit
  where sBit = newSequence worker timestamp
        wBit = (worker ^. workerId) `shiftL` wShift
        dBit = (worker ^. datacenterId) `shiftL` dShift
        tBit = (timestamp - worker ^. conf . timestampBase) `shiftL` tShift
        wShift = worker ^. conf . sequenceBits
        dShift = wShift + (worker ^. conf . workerIdBits)
        tShift = dShift + (worker ^. conf . datacenterIdBits)

sequenceIsFull :: IdWorker -> Timestamp -> Bool
sequenceIsFull worker timestamp = timestamp == (worker ^. lastTimestamp) && (worker ^. sequence) == 2^(worker ^. conf . sequenceBits) - 1

newSequence :: IdWorker -> Timestamp -> Int64
newSequence worker timestamp =
  if timestamp == (worker ^. lastTimestamp)
     then (worker ^. sequence) + 1
     else 0
