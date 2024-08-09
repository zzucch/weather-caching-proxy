module Lib.Internal.Utils.Time
  ( isCurrentTime,
    secondsToMicroseconds,
  )
where

import Data.Time.Clock.POSIX

isCurrentTime :: Int -> Int -> IO Bool
isCurrentTime timestamp offset = do
  currentTime <- round <$> getPOSIXTime
  return $
    timestamp >= (currentTime - offset)
      && timestamp <= (currentTime + offset)

secondsToMicroseconds :: Integer -> Integer
secondsToMicroseconds seconds = seconds * 1000000
