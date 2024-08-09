{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Server
  ( app,
  )
where

import Control.Monad.IO.Class
import Data.Maybe
import Lib.Internal.Caching.Cache (WeatherResponse)
import Lib.Internal.DataFetching.DataFetch
import Lib.Internal.Utils.Concurrency (waitForFirstNonNothingResult)
import Lib.Internal.Utils.Time (isCurrentTime)
import Lib.Util
import Network.Wai
import Servant
import Prelude

type WeatherAPI =
  "weather"
    :> QueryParam "latitude" Double
    :> QueryParam "longitude" Double
    :> QueryParam "time" Int
    :> Get '[JSON] WeatherResponse

notFoundError :: ServerError
notFoundError =
  err404
    { errBody = "Could not retrieve weather data"
    }

invalidParametersError :: ServerError
invalidParametersError =
  err400
    { errBody = "Invalid parameters"
    }

getWeatherData ::
  ExternalAPIParams ->
  WeatherParams ->
  WeatherOffsets ->
  Int ->
  Handler (Maybe WeatherResponse)
getWeatherData
  apiParams
  params
  offsets
  currentTimeOffset = do
    isCurrent <-
      liftIO $
        isCurrentTime
          (timestamp params)
          currentTimeOffset
    if isCurrent
      then
        liftIO $
          waitForFirstNonNothingResult
            ( getCachedData
                params
                offsets
            )
            ( getFromRemoteAndCacheData
                apiParams
                (locationParams params)
                offsets
            )
      else
        liftIO $
          getCachedData
            params
            offsets

server ::
  ExternalAPIParams ->
  WeatherOffsets ->
  Int ->
  Server WeatherAPI
server
  apiParams
  offsets
  currentTimeOffset = weather
    where
      weather ::
        Maybe Double ->
        Maybe Double ->
        Maybe Int ->
        Handler WeatherResponse
      weather (Just latitude') (Just longitude') (Just time') = do
        res <-
          getWeatherData
            apiParams
            ( WeatherParams
                { locationParams =
                    LocationParams
                      { latitude = latitude',
                        longitude = longitude'
                      },
                  timestamp = time'
                }
            )
            offsets
            currentTimeOffset
        maybe (throwError notFoundError) return res
      weather _ _ _ =
        throwError invalidParametersError

weatherAPI :: Proxy WeatherAPI
weatherAPI = Proxy

app ::
  ExternalAPIParams ->
  WeatherOffsets ->
  Int ->
  Application
app
  apiParams
  offsets
  currentTimeOffset =
    serve weatherAPI $
      server
        apiParams
        offsets
        currentTimeOffset
