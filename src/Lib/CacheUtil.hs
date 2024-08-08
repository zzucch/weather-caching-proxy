{-# LANGUAGE OverloadedStrings #-}

module Lib.CacheUtil
  ( weatherDataKey,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

discretizeByOffset :: Double -> Double -> Int
discretizeByOffset value offset = floor (value / offset)

discretizeTimestamp :: Int -> Int -> Int
discretizeTimestamp timestamp interval' = timestamp `div` interval'

weatherDataKey :: Double -> Double -> Int -> ByteString
weatherDataKey latitude' longitude' timestamp =
  pack $
    show (discretizeByOffset latitude' 0.01)
      ++ ":"
      ++ show (discretizeByOffset longitude' 0.01)
      ++ ":"
      ++ show (discretizeTimestamp timestamp 300)
