module Lib.Config
  ( getApiKey,
  )
where

import System.Environment (lookupEnv)
import System.Exit (exitFailure)

getApiKey :: IO String
getApiKey = do
  apiKey <- lookupEnv "API_KEY"
  case apiKey of
    Just key -> return key
    Nothing -> do
      putStrLn "Error: API_KEY environment variable not set"
      exitFailure
