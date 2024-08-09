{-# LANGUAGE OverloadedStrings #-}

module Lib.Internal.Caching.CacheUtil
  ( weatherDataKey,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Lib.Util

discretizeByOffset :: Double -> Double -> Int
discretizeByOffset value offset = floor (value / offset)

discretizeTimestamp :: Int -> Int -> Int
discretizeTimestamp timestamp' offset = timestamp' `div` offset

weatherDataKey ::
  WeatherParams ->
  WeatherOffsets ->
  ByteString
weatherDataKey params offsets =
  pack $
    show
      ( discretizeByOffset
          (latitude $ locationParams params)
          (latitudeOffset offsets)
      )
      ++ ":"
      ++ show
        ( discretizeByOffset
            (longitude $ locationParams params)
            (longitudeOffset offsets)
        )
      ++ ":"
      ++ show
        ( discretizeTimestamp
            (timestamp params)
            (timestampOffset offsets)
        )
