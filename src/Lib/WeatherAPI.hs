{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.WeatherAPI
  ( WeatherResponse (..),
    WeatherData (..),
    getWeatherData,
  )
where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client (ClientM, client)

data WeatherResponse = WeatherResponse
  { lat :: Double,
    lon :: Double,
    generationtime_ms :: Double,
    utc_offset_seconds :: Int,
    timezone :: String,
    timezone_abbreviation :: String,
    elevation :: Double,
    current :: WeatherData
  }
  deriving (Show, Generic)

instance FromJSON WeatherResponse where
  parseJSON = withObject "WeatherResponse" $ \v ->
    WeatherResponse
      <$> v .: fromString "latitude"
      <*> v .: fromString "longitude"
      <*> v .: fromString "generationtime_ms"
      <*> v .: fromString "utc_offset_seconds"
      <*> v .: fromString "timezone"
      <*> v .: fromString "timezone_abbreviation"
      <*> v .: fromString "elevation"
      <*> v .: fromString "current"

data WeatherData = WeatherData
  { time :: Int,
    interval :: Int,
    temperature_2m :: Double,
    relative_humidity_2m :: Int,
    apparent_temperature :: Double,
    is_day :: Int,
    precipitation :: Double,
    rain :: Int,
    showers :: Double,
    snowfall :: Int,
    weather_code :: Int,
    cloud_cover :: Int,
    pressure_msl :: Double,
    surface_pressure :: Double,
    wind_speed_10m :: Double,
    wind_direction_10m :: Int,
    wind_gusts_10m :: Double
  }
  deriving (Show, Generic)

instance FromJSON WeatherData

type WeatherAPI =
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

weatherAPI :: Proxy WeatherAPI
weatherAPI = Proxy

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
getWeatherData latitude longitude apiKey =
  client
    weatherAPI
    (Just latitude)
    (Just longitude)
    (Just currentParams)
    (Just windSpeedUnit)
    (Just timeFormat)
    (Just forecastDays)
    (if apiKey /= "" then Just apiKey else Nothing)
