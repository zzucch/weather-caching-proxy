module Lib.DataFetch
  ( getWeatherData,
  )
where

import Control.Monad.IO.Class
import Data.Maybe
import Database.Redis
import Lib.Cache (WeatherResponse, findWeatherResponse)
import Lib.Concurrency (waitForFirstNonNothingResult)
import Lib.QueryAPI (getWeatherResponse)
import Lib.Time (isCurrentTime)
import Servant
import Prelude

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
