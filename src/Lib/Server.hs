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

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Maybe
import Database.Redis
import Lib.Cache (WeatherResponse, findWeatherResponse)
import Lib.QueryAPI (getWeatherResponse)
import Network.Wai
import Servant
import Prelude

type WeatherAPI =
  "weather"
    :> QueryParam "latitude" Double
    :> QueryParam "longitude" Double
    :> QueryParam "time" Int
    :> Get '[JSON] WeatherResponse

server :: Server WeatherAPI
server = weather
  where
    weather ::
      Maybe Double ->
      Maybe Double ->
      Maybe Int ->
      Handler WeatherResponse
    weather mlatitude mlongitude mtime =
      case (mlatitude, mlongitude, mtime) of
        (Just lat, Just lon, Just t) -> do
          res <-
            liftIO $ race
              (liftIO $ getFromCache lat lon t)
              (getFromRemote lat lon)
          case res of
            Left (Just weatherData) -> return weatherData
            Right (Just weatherData) -> return weatherData
            _ ->
              throwError
                err404
                  { errBody = "could not retrieve weather data"
                  }
        _ ->
          throwError
            err400
              { errBody = "invalid parameters"
              }

weatherAPI :: Proxy WeatherAPI
weatherAPI = Proxy

app :: Application
app = serve weatherAPI server

getFromCache :: Double -> Double -> Int -> IO (Maybe WeatherResponse)
getFromCache lat lon t = do
  connection <- liftIO $ connect defaultConnectInfo
  findWeatherResponse connection lat lon t

getFromRemote :: Double -> Double -> IO (Maybe WeatherResponse)
getFromRemote lat lon = do
  getWeatherResponse lat lon
