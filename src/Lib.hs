module Lib
  ( runServer,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Lib.Config (getApiKey)
import Lib.WeatherAPI (WeatherResponse (current), getWeatherData)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client
  ( BaseUrl (BaseUrl),
    Scheme (Https),
    mkClientEnv,
    runClientM,
  )

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
