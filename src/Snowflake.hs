{-# LANGUAGE TemplateHaskell #-}
module Snowflake
  ( Timestamp
  , Conf(..)
  , IdWorker(..)
  , defaultConf
  , next
  , nexts
  ) where


import Prelude hiding (sequence)
import Data.Int (Int64)
import Data.Maybe (Maybe, isNothing, fromJust)
import Data.Bits ( (.|.)
                 , shiftL
                 )
import Data.Time.Exts.Unix ( getCurrentUnixDateTimeMillis
                           , UnixDateTimeMillis(..)
                           )

type Timestamp = Int64

data Conf = Conf { _sequenceBits :: Int
                 , _workerIdBits :: Int
                 , _datacenterIdBits :: Int
                 , _twepoch :: Timestamp  -- timestamp base-line
                 } deriving (Show)

data IdWorker = IdWorker { _sequence :: Int64
                         , _workerId :: Int64
                         , _datacenterId :: Int64
                         , _timestamp :: Timestamp
                         , _conf :: Conf
                         } deriving (Show)

defaultConf = Conf { _sequenceBits = 12
                   , _workerIdBits = 5
                   , _datacenterIdBits = 5
                   , _twepoch = 1472733628921
                   }

next :: IdWorker -> Timestamp -> (Maybe Int64, IdWorker)
next worker@(IdWorker lastSq wid did lastTs (Conf sBits wBits dBits twepoch)) ts
  | lastTs == ts && lastSq == 2^sBits - 1 = (Nothing, worker)
  | otherwise = (Just newId, worker {_sequence = newSq, _timestamp = ts})
    where
      newSq = if lastTs == ts then lastSq + 1 else 0
      newId = tBit .|. dBit .|. wBit .|. newSq
      wBit = wid `shiftL` sBits
      dBit = did `shiftL` (sBits + wBits)
      tBit = (ts - twepoch) `shiftL` (sBits + wBits + dBits)

nexts :: IdWorker -> Int -> IO ([Int64], IdWorker)
nexts worker 0 = return ([], worker)
nexts worker n = do
  timestamp <- _udt_mil_base <$> getCurrentUnixDateTimeMillis
  let (maybeNewId, newWorker) = next worker timestamp in
   if isNothing maybeNewId
      then nexts worker n
      else (\(ids, wk) -> (fromJust maybeNewId : ids, wk)) <$> nexts newWorker (n-1)
