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

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Maybe
import Database.Redis
import Lib.Cache (WeatherResponse, findWeatherResponse)
import Lib.QueryAPI (getWeatherResponse)
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

getRemoteData ::
  Double ->
  Double ->
  IO (Maybe WeatherResponse)
getRemoteData latitude longitude = do
  getWeatherResponse latitude longitude

getCachedData ::
  Double ->
  Double ->
  Int ->
  IO (Maybe WeatherResponse)
getCachedData latitude longitude time' = do
  connection <- liftIO $ connect defaultConnectInfo
  findWeatherResponse connection latitude longitude time'

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

waitForFirstNonNothingResult ::
  IO (Maybe a) ->
  IO (Maybe a) ->
  IO (Maybe a)
waitForFirstNonNothingResult action1 action2 = do
  resultVar <- newEmptyMVar
  let setResult x = case x of
        Just y -> tryPutMVar resultVar (Just y)
        Nothing -> return False

  _ <-
    concurrently
      (action1 >>= setResult)
      (action2 >>= setResult)
  takeMVar resultVar

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
