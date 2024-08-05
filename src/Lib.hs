{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( runServer,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientM,
    Scheme (Https),
    client,
    mkClientEnv,
    runClientM,
  )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

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
    rain :: Double,
    showers :: Double,
    snowfall :: Double,
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
    :> Get '[JSON] WeatherResponse

weatherAPI :: Proxy WeatherAPI
weatherAPI = Proxy

getWeatherData :: Double -> Double -> String -> ClientM WeatherResponse
getWeatherData latitude longitude apiKey =
  case apiKey of
    "" ->
      client
        weatherAPI
        (Just latitude)
        (Just longitude)
        ( Just $
            "temperature_2m,relative_humidity_2m,"
              ++ "apparent_temperature,is_day,precipitation,"
              ++ "rain,showers,snowfall,weather_code,cloud_cover,"
              ++ "pressure_msl,surface_pressure,wind_speed_10m,"
              ++ "wind_direction_10m,wind_gusts_10m"
        )
        (Just "ms")
        (Just "unixtime")
        (Just 1)
    _ -> undefined

getApiKey :: IO String
getApiKey = do
  apiKey <- lookupEnv "API_KEY"
  case apiKey of
    Just key -> return key
    Nothing -> do
      putStrLn "Error: API_KEY environment variable not set"
      exitFailure

runServer :: IO ()
runServer = do
  manager <- newTlsManager
  loadFile defaultConfig

  let apiBaseUrl = BaseUrl Https "api.open-meteo.com" 443 ""
  let clientEnv = mkClientEnv manager apiBaseUrl
  let apiLat = 39.099724
  let apiLon = -94.578331
  apiAppid <- getApiKey

  res <-
    runClientM
      (getWeatherData apiLat apiLon apiAppid)
      clientEnv

  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right weatherResponse -> print $ current weatherResponse
