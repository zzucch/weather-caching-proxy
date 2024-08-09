{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Internal.DataFetching.DataFetch
  ( getFromRemoteAndCacheData,
    getCachedData,
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class
import Data.Maybe
import Database.Redis
import Lib.Internal.Caching.Cache
  ( WeatherResponse,
    addWeatherResponse,
    findWeatherResponse,
  )
import Lib.Internal.DataFetching.QueryAPI (getWeatherResponse)
import Lib.Util
  ( ExternalAPIParams,
    LocationParams (..),
    WeatherOffsets,
    WeatherParams,
  )
import Prelude

getFromRemoteAndCacheData ::
  ExternalAPIParams ->
  LocationParams ->
  WeatherOffsets ->
  IO (Maybe WeatherResponse)
getFromRemoteAndCacheData
  apiParams
  locationParams
  offsets = do
    res <-
      getWeatherResponse apiParams locationParams
    case res of
      Just response -> do
        _ <- forkIO $ do
          connection <- connect defaultConnectInfo
          _ <-
            addWeatherResponse
              connection
              response
              offsets
          return ()
        return res
      Nothing -> return res

getCachedData ::
  WeatherParams ->
  WeatherOffsets ->
  IO (Maybe WeatherResponse)
getCachedData
  params
  offsets = do
    result <- try $ do
      connection <- liftIO $ connect defaultConnectInfo
      findWeatherResponse
        connection
        params
        offsets
    case result of
      Right weatherResponse -> return weatherResponse
      Left (e :: SomeException) -> do
        putStrLn $ "Error connecting to Redis: " ++ show e
        return Nothing
