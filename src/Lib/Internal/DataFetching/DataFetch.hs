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
import Debug.Trace (trace)
import Lib.Internal.Caching.Cache
  ( WeatherResponse,
    addWeatherResponse,
    findWeatherResponse,
  )
import Lib.Internal.DataFetching.QueryAPI (getWeatherResponse)
import Prelude

getFromRemoteAndCacheData ::
  String ->
  Double ->
  Double ->
  IO (Maybe WeatherResponse)
getFromRemoteAndCacheData apiKey latitude longitude = do
  res <-
    getWeatherResponse apiKey latitude longitude
  case res of
    Just response -> do
      _ <- forkIO $ do
        trace ("forking.. response is: " ++ show response) return ()
        connection <- connect defaultConnectInfo
        _ <- addWeatherResponse connection response
        return ()
      return res
    Nothing -> return res

getCachedData ::
  Double ->
  Double ->
  Int ->
  IO (Maybe WeatherResponse)
getCachedData latitude longitude time' = do
  result <- try $ do
    connection <- liftIO $ connect defaultConnectInfo
    findWeatherResponse connection latitude longitude time'
  case result of
    Right weatherResponse -> return weatherResponse
    Left (e :: SomeException) -> do
      putStrLn $ "Error connecting to Redis: " ++ show e
      return Nothing
