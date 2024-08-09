{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class
import Lib.Autocache
import Lib.Config
import Lib.Server
import Lib.Util
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  config <- getConfig
  apiParams <- liftIO $ getExternalAPIParams config
  let port' = fromIntegral $ port config
      locations = getAutoCacheLocationParams config
      period = fromIntegral $ autoCachePeriodSeconds config
      currentTimeOffset = fromIntegral $ currentTimeOffsetSeconds config
      offsets' =
        WeatherOffsets
          { latitudeOffset = config.offsets.latitudeOffsetDegrees,
            longitudeOffset = config.offsets.longitudeOffsetDegrees,
            timestampOffset =
              fromIntegral
                config.offsets.timestampOffsetSeconds
          }
  _ <-
    forkIO $
      run
        port'
        ( app
            apiParams
            offsets'
            currentTimeOffset
        )
  startAutoCaching
    apiParams
    offsets'
    locations
    period
