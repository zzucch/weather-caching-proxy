{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Lib.Util

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
  WeatherOffsets ->
  IO (Either Reply Status)
addWeatherResponse
  connection
  weather
  offsets =
    runRedis connection $ do
      let key =
            weatherDataKey
              WeatherParams
                { timestamp = weather.current.time,
                  locationParams =
                    LocationParams
                      { Lib.Util.longitude = weather.longitude,
                        Lib.Util.latitude = weather.latitude
                      }
                }
              offsets
          value = toStrict $ encode weather

      set key value

findWeatherResponse ::
  Connection ->
  WeatherParams ->
  WeatherOffsets ->
  IO (Maybe WeatherResponse)
findWeatherResponse
  connection
  params
  offsets =
    runRedis connection $ do
      let key =
            weatherDataKey
              params
              offsets

      res <- get key

      return $ case res of
        Right (Just value) -> decode $ fromStrict value
        _ -> Nothing
