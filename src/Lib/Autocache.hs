module Lib.Autocache
  ( startAutoCaching,
  )
where

import Control.Concurrent.Thread.Delay
import Control.Monad (forever)
import Control.Monad.IO.Class
import Lib.Internal.DataFetching.DataFetch (getFromRemoteAndCacheData)
import Lib.Internal.Utils.Time (secondsToMicroseconds)
import Lib.Util

startAutoCaching ::
  ExternalAPIParams ->
  WeatherOffsets ->
  [LocationParams] ->
  Integer ->
  IO ()
startAutoCaching
  apiParams
  offsets
  locations
  period = do
    forever $ do
      _ <-
        liftIO $
          traverse
            ( \location ->
                getFromRemoteAndCacheData
                  apiParams
                  location
                  offsets
            )
            locations
      delay $ secondsToMicroseconds period
