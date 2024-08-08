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
import Lib.Cache (WeatherResponse)
import Lib.Concurrency (waitForFirstNonNothingResult)
import Lib.DataFetch
import Lib.Time (isCurrentTime)
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
  Double ->
  Double ->
  Int ->
  Handler (Maybe WeatherResponse)
getWeatherData latitude longitude time' = do
  isCurrent <- liftIO $ isCurrentTime time'
  if isCurrent
    then
      liftIO $
        waitForFirstNonNothingResult
          (getCachedData latitude longitude time')
          (getRemoteData latitude longitude)
    else
      liftIO $
        getCachedData latitude longitude time'

server :: Server WeatherAPI
server = weather
  where
    weather ::
      Maybe Double ->
      Maybe Double ->
      Maybe Int ->
      Handler WeatherResponse
    weather (Just latitude) (Just longitude) (Just time') = do
      res <-
        getWeatherData
          latitude
          longitude
          time'
      maybe (throwError notFoundError) return res
    weather _ _ _ =
      throwError invalidParametersError

weatherAPI :: Proxy WeatherAPI
weatherAPI = Proxy

app :: Application
app = serve weatherAPI server
