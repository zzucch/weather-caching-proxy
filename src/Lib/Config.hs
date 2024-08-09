{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Config
  ( Config (..),
    Coordinate (..),
    Offsets (..),
    getConfig,
    getApiKey,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (unless, when)
import Data.Vector (filter)
import Dhall
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

data Coordinate = Coordinate
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Generic, Show)

instance FromDhall Coordinate

data Offsets = Offsets
  { latitudeOffsetDegrees :: Double,
    longitudeOffsetDegrees :: Double,
    timestampOffsetSeconds :: Natural
  }
  deriving (Generic, Show)

instance FromDhall Offsets

data Config = Config
  { port :: Natural,
    offsets :: Offsets,
    currentTimeOffsetSeconds :: Natural,
    autoCacheCoordinates :: Vector Coordinate,
    autoCachePeriodSeconds :: Natural,
    customExternalAPIDomain :: Maybe String
  }
  deriving (Generic, Show)

instance FromDhall Config

getConfig :: IO Config
getConfig = do
  x <- input auto "./config.dhall"
  validateConfig (x :: Config)

validateConfig :: Config -> IO Config
validateConfig config = do
  validateOffsets (offsets config)
  validateAutoCachePeriod (autoCachePeriodSeconds config)
  validateCoordinates (autoCacheCoordinates config)
  return config

validateOffsets :: Offsets -> IO ()
validateOffsets offsets' = do
  let latOffset = latitudeOffsetDegrees offsets'
      lonOffset = longitudeOffsetDegrees offsets'
  when
    ( abs latOffset > 90
        || abs lonOffset > 180
    )
    $ do
      putStrLn "Error: Latitude offset must be between -90 and 90, longitude offset must be between -180 and 180"
      exitFailure

validateAutoCachePeriod :: Natural -> IO ()
validateAutoCachePeriod period = do
  when (period <= 0) $ do
    putStrLn "Error: Auto cache period must be greater than 0"
    exitFailure

validateCoordinates :: Vector Coordinate -> IO ()
validateCoordinates coordinates = do
  let invalidCoordinates =
        Data.Vector.filter
          ( \c ->
              abs (latitude c) > 90
                || abs (longitude c) > 180
          )
          coordinates
  unless (null invalidCoordinates) $ do
    putStrLn "Error: Invalid coordinates found in autoCacheCoordinates. Latitude must be between -90 and 90, longitude must be between -180 and 180."
    exitFailure

getApiKey :: IO String
getApiKey = do
  loadFile defaultConfig
  apiKey <- lookupEnv "API_KEY"
  case apiKey of
    Just key -> return key
    Nothing -> do
      putStrLn "Error: API_KEY environment variable not set"
      exitFailure
