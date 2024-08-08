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
  String ->
  Double ->
  Double ->
  Int ->
  Handler (Maybe WeatherResponse)
getWeatherData apiKey latitude longitude time' = do
  isCurrent <- liftIO $ isCurrentTime time'
  if isCurrent
    then
      liftIO $
        waitForFirstNonNothingResult
          (getCachedData latitude longitude time')
          (getFromRemoteAndCacheData apiKey latitude longitude)
    else
      liftIO $
        getCachedData latitude longitude time'

server :: String -> Server WeatherAPI
server apiKey = weather
  where
    weather ::
      Maybe Double ->
      Maybe Double ->
      Maybe Int ->
      Handler WeatherResponse
    weather (Just latitude) (Just longitude) (Just time') = do
      res <-
        getWeatherData
          apiKey
          latitude
          longitude
          time'
      maybe (throwError notFoundError) return res
    weather _ _ _ =
      throwError invalidParametersError

weatherAPI :: Proxy WeatherAPI
weatherAPI = Proxy

app :: String -> Application
app apiKey = serve weatherAPI $ server apiKey
