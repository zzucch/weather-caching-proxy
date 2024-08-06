{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.QueryAPI
  ( getWeatherData,
  )
where

import Data.Aeson
import Data.Aeson.Key
import Data.Proxy
import GHC.Generics
import Lib.Cache
import Servant.API
import Servant.Client (ClientM, client)

newtype WeatherResponseAPI = WeatherResponseAPI
  { unwrapWeatherResponse :: WeatherResponse
  }
  deriving (Show, Eq, Generic)

instance FromJSON WeatherResponseAPI where
  parseJSON = withObject "WeatherResponseAPI" $ \v ->
    WeatherResponseAPI
      <$> ( WeatherResponse
              <$> v .: fromString "latitude"
              <*> v .: fromString "longitude"
              <*> v .: fromString "generationtime_ms"
              <*> v .: fromString "utc_offset_seconds"
              <*> v .: fromString "elevation"
              <*> v .: fromString "current"
          )

type ExternalWeatherAPI =
  "v1"
    :> "forecast"
    :> QueryParam "latitude" Double
    :> QueryParam "longitude" Double
    :> QueryParam "current" String
    :> QueryParam "wind_speed_unit" String
    :> QueryParam "timeformat" String
    :> QueryParam "forecast_days" Int
    :> QueryParam "apikey" String
    :> Get '[JSON] WeatherResponse

externalWeatherAPI :: Proxy ExternalWeatherAPI
externalWeatherAPI = Proxy

currentParams :: String
currentParams =
  "temperature_2m,relative_humidity_2m,"
    ++ "apparent_temperature,is_day,precipitation,"
    ++ "rain,showers,snowfall,weather_code,cloud_cover,"
    ++ "pressure_msl,surface_pressure,wind_speed_10m,"
    ++ "wind_direction_10m,wind_gusts_10m"

windSpeedUnit :: String
windSpeedUnit = "ms"

timeFormat :: String
timeFormat = "unixtime"

forecastDays :: Int
forecastDays = 1

getWeatherData :: Double -> Double -> String -> ClientM WeatherResponse
getWeatherData latitude' longitude' apiKey =
  client
    externalWeatherAPI
    (Just latitude')
    (Just longitude')
    (Just currentParams)
    (Just windSpeedUnit)
    (Just timeFormat)
    (Just forecastDays)
    (if apiKey /= "" then Just apiKey else Nothing)
