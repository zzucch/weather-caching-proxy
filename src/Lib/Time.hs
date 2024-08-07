module Lib.Time
  ( isCurrentTime,
  )
where

import Data.Time.Clock.POSIX

offset :: Int
offset = 60

isCurrentTime :: Int -> IO Bool
isCurrentTime timestamp = do
  currentTime <- round <$> getPOSIXTime
  return $
    timestamp >= (currentTime - offset)
      && timestamp <= (currentTime + offset)
