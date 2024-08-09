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

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Data.Swagger
import Lib.Internal.Caching.Cache (WeatherResponse)
import Lib.Internal.DataFetching.DataFetch
import Lib.Internal.Utils.Concurrency (waitForFirstNonNothingResult)
import Lib.Internal.Utils.Time (isCurrentTime)
import Lib.Util
import Network.Wai
import Servant
import Servant.Swagger
import Prelude

swaggerSpec :: Swagger
swaggerSpec =
  toSwagger weatherAPI
    & info . title .~ "Weather API"
    & info . version .~ "1.0"
    & info . description ?~ "API for retrieving weather data"

type WeatherAPI =
  "weather"
    :> QueryParam "latitude" Double
    :> QueryParam "longitude" Double
    :> QueryParam "time" Int
    :> Get '[JSON] WeatherResponse

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> WeatherAPI

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

weatherServer ::
  ExternalAPIParams ->
  WeatherOffsets ->
  Int ->
  Server WeatherAPI
weatherServer
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

server :: ExternalAPIParams -> WeatherOffsets -> Int -> Server API
server apiParams offsets currentTimeOffset =
  return swaggerSpec
    :<|> weatherServer
      apiParams
      offsets
      currentTimeOffset

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
    serve (Proxy :: Proxy API) $
      server
        apiParams
        offsets
        currentTimeOffset
