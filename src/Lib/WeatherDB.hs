{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.WeatherDB
  ( addWeatherData,
    findWeatherData,
  )
where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Database.Redis (Connection, Reply, Status, get, runRedis, set)
import GHC.Generics (Generic)

data WeatherData = WeatherData
  { longitude :: Double,
    latitude :: Double,
    timestamp :: Int,
    temperature :: Double,
    apparent_temperature :: Double,
    relative_humidity :: Int,
    is_day :: Int,
    precipitation :: Int,
    rain :: Int,
    showers :: Int,
    snowfall :: Int,
    weather_code :: Int,
    cloud_cover :: Int,
    pressure_mean_sea_level :: Double,
    surface_pressure :: Double,
    wind_speed :: Double,
    wind_direction :: Int,
    wind_gusts :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON WeatherData

instance ToJSON WeatherData

weatherDataKey :: Double -> Double -> Int -> ByteString
weatherDataKey lon lat time =
  pack $
    "weather:"
      ++ show lon
      ++ ":"
      ++ show lat
      ++ ":"
      ++ show time

addWeatherData :: Connection -> WeatherData -> IO (Either Reply Status)
addWeatherData connection weather = runRedis connection $ do
  let key =
        weatherDataKey
          (longitude weather)
          (latitude weather)
          (timestamp weather)
      value = toStrict $ encode weather
  set key value

findWeatherData ::
  Connection ->
  Double ->
  Double ->
  Int ->
  IO (Maybe WeatherData)
findWeatherData connection lon lat time = runRedis connection $ do
  let key = weatherDataKey lon lat time
  res <- get key
  return $ case res of
    Right (Just val) -> decode $ fromStrict val
    _ -> Nothing
