module Lib
  ( runServer,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Lib.Config (getApiKey)
import Lib.WeatherAPI (WeatherResponse (current), getWeatherData)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientEnv,
    Scheme (Https),
    mkClientEnv,
    runClientM,
  )

createClientEnv :: Manager -> IO ClientEnv
createClientEnv manager = do
  mkClientEnv manager . apiBaseUrl <$> getApiKey

apiBaseUrl :: String -> BaseUrl
apiBaseUrl apiAppid =
  case apiAppid of
    "" -> BaseUrl Https "api.open-meteo.com" 443 ""
    _ -> BaseUrl Https "customer-api.open-meteo.com" 443 ""

runServer :: IO ()
runServer = do
  manager <- newTlsManager
  loadFile defaultConfig

  let latitude = 39.099724
  let longitude = -94.578331
  apiKey <- getApiKey

  clientEnv <- createClientEnv manager

  res <-
    runClientM
      (getWeatherData latitude longitude apiKey)
      clientEnv

  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right weatherResponse -> print $ current weatherResponse
