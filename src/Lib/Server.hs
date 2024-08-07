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
import Database.Redis
import Debug.Trace
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

server :: Server WeatherAPI
server = weather
  where
    weather ::
      Maybe Double ->
      Maybe Double ->
      Maybe Int ->
      Handler WeatherResponse
    weather mlatitude mlongitude mtime = do
      case (mlatitude, mlongitude, mtime) of
        (Just lat, Just lon, Just t) -> do
          isCurrent <- liftIO $ isCurrentTime t
          if isCurrent
            then do
              remoteData <- liftIO $ getFromRemote lat lon
              case remoteData of
                Just weatherData -> return weatherData
                Nothing -> do
                  cachedData <- liftIO $ getFromCache lat lon t
                  case cachedData of
                    Just weatherData -> return weatherData
                    Nothing ->
                      throwError
                        err404
                          { errBody = "could not retrieve weather data"
                          }
            else do
              cachedData <- liftIO $ getFromCache lat lon t
              case cachedData of
                Just weatherData -> return weatherData
                Nothing ->
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
  res <- findWeatherResponse connection lat lon t
  trace ("cache: " ++ show res) return ()
  return res

getFromRemote :: Double -> Double -> IO (Maybe WeatherResponse)
getFromRemote lat lon = do
  trace "getting remote response" return ()
  res <- getWeatherResponse lat lon
  trace ("remote: " ++ show res) return ()
  return res
