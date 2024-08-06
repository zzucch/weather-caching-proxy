module Lib.DataConverter
  ( weatherResponseToWeatherData,
  )
where

import qualified Lib.QueryAPI as API
import qualified Lib.Cache as DB

weatherResponseToWeatherData :: API.WeatherResponse -> DB.WeatherData
weatherResponseToWeatherData response =
  DB.WeatherData
    { DB.latitude = API.lat response,
      DB.longitude = API.lon response,
      DB.timestamp = API.time current,
      DB.temperature = API.temperature_2m current,
      DB.apparent_temperature = API.apparent_temperature current,
      DB.relative_humidity = API.relative_humidity_2m current,
      DB.is_day = API.is_day current,
      DB.precipitation = API.precipitation current,
      DB.rain = API.rain current,
      DB.showers = API.showers current,
      DB.snowfall = API.snowfall current,
      DB.weather_code = API.weather_code current,
      DB.cloud_cover = API.cloud_cover current,
      DB.pressure_mean_sea_level = API.pressure_msl current,
      DB.surface_pressure = API.surface_pressure current,
      DB.wind_speed = API.wind_speed_10m current,
      DB.wind_direction = API.wind_direction_10m current,
      DB.wind_gusts = API.wind_gusts_10m current
    }
  where
    current = API.current response
