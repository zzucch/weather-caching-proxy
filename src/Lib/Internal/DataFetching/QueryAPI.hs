{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Internal.DataFetching.QueryAPI
  ( getWeatherResponse,
  )
where

import Data.Aeson
import Data.Aeson.Key
import Data.Proxy
import GHC.Generics
import Lib.Internal.Caching.Cache
import Lib.Util (ExternalAPIParams (..), LocationParams (..))
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client
  ( BaseUrl (..),
    ClientEnv,
    ClientM,
    Scheme (Https),
    client,
    mkClientEnv,
    runClientM,
  )

newtype WeatherResponseAPI = WeatherResponseAPI
  { unwrapWeatherResponse :: WeatherResponse
  }
  deriving (Show, Eq, Generic)

instance FromJSON WeatherResponseAPI where
  parseJSON =
    withObject "WeatherResponseAPI" $ \v ->
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

ifNotEmpty :: String -> Maybe String
ifNotEmpty x = if x /= "" then Just x else Nothing

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

getWeatherResponseClientM ::
  LocationParams ->
  String ->
  ClientM WeatherResponse
getWeatherResponseClientM locationParams apiKey' =
  client
    externalWeatherAPI
    (Just $ Lib.Util.latitude locationParams)
    (Just $ Lib.Util.longitude locationParams)
    (Just currentParams)
    (Just windSpeedUnit)
    (Just timeFormat)
    (Just forecastDays)
    (ifNotEmpty apiKey')

apiBaseUrl :: String -> BaseUrl
apiBaseUrl domainName' =
  case domainName' of
    "" -> BaseUrl Https "api.open-meteo.com" 443 ""
    _ -> BaseUrl Https domainName' 443 ""

createClientEnv :: Manager -> String -> IO ClientEnv
createClientEnv manager domainName' =
  return $ mkClientEnv manager (apiBaseUrl domainName')

getWeatherResponse ::
  ExternalAPIParams ->
  LocationParams ->
  IO (Maybe WeatherResponse)
getWeatherResponse apiParams locationParams = do
  manager <- newTlsManager
  clientEnv <- createClientEnv manager $ domainName apiParams
  res <-
    runClientM
      ( getWeatherResponseClientM
          locationParams
          (apiKey apiParams)
      )
      clientEnv
  case res of
    Right weatherResponse -> return (Just weatherResponse)
    Left _ -> return Nothing
