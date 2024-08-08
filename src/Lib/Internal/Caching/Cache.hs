{-# LANGUAGE DeriveGeneric #-}

module Lib.Internal.Caching.Cache
  ( addWeatherResponse,
    findWeatherResponse,
    WeatherResponse (..),
    WeatherData (..),
  )
where

import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Database.Redis (Connection, Reply, Status, get, runRedis, set)
import GHC.Generics (Generic)
import Lib.Internal.Caching.CacheUtil

data WeatherResponse = WeatherResponse
  { latitude :: Double,
    longitude :: Double,
    generationtime_ms :: Double,
    utc_offset_seconds :: Int,
    elevation :: Double,
    current :: WeatherData
  }
  deriving (Show, Eq, Generic)

instance FromJSON WeatherResponse

instance ToJSON WeatherResponse

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
  deriving (Show, Eq, Generic)

instance FromJSON WeatherData

instance ToJSON WeatherData

addWeatherResponse ::
  Connection ->
  WeatherResponse ->
  IO (Either Reply Status)
addWeatherResponse connection weather =
  runRedis connection $ do
    let key =
          weatherDataKey
            (latitude weather)
            (longitude weather)
            (time $ current weather)
        value = toStrict $ encode weather

    set key value

findWeatherResponse ::
  Connection ->
  Double ->
  Double ->
  Int ->
  IO (Maybe WeatherResponse)
findWeatherResponse connection latitude' longitude' timestamp =
  runRedis connection $ do
    let key =
          weatherDataKey
            latitude'
            longitude'
            timestamp

    res <- get key

    return $ case res of
      Right (Just value) -> decode $ fromStrict value
      _ -> Nothing
