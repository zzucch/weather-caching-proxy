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
import Lib.Cache (findWeatherResponse, WeatherResponse)
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
          connection <-
            liftIO $ connect defaultConnectInfo

          maybeWeatherData <-
            liftIO $ findWeatherResponse connection lat lon t

          case maybeWeatherData of
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
