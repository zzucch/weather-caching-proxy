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

import Data.Maybe
import Lib.Cache (WeatherResponse)
import Lib.DataFetch (getWeatherData)
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
