module Lib
  ( runServer,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Database.Redis (connect, defaultConnectInfo)
import Lib.Cache
import Lib.Config (getApiKey)
import Lib.QueryAPI (getWeatherData)
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
createClientEnv manager =
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

  let lat = 28.6448
  let lon = 77.216721
  apiKey <- getApiKey

  clientEnv <- createClientEnv manager

  res <-
    runClientM
      (getWeatherData lat lon apiKey)
      clientEnv

  either handleError handleSuccess res

handleError :: (Show err) => err -> IO ()
handleError err = putStrLn $ "Error: " ++ show err

handleSuccess :: WeatherResponse -> IO ()
handleSuccess weather = do
  print $ "weather: " ++ show weather

  connection <- connect defaultConnectInfo

  addResult <- addWeatherResponse connection weather
  print $ "addWeather: " ++ show addResult

  foundWeather <-
    findWeatherResponse
      connection
      (latitude weather)
      (longitude weather)
      (time $ current weather)

  print $ "findWeather: " ++ show foundWeather
