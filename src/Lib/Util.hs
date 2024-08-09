module Lib.Util
  ( ExternalAPIParams (..),
    WeatherParams (..),
    LocationParams (..),
    WeatherOffsets (..),
    getAutoCacheLocationParams,
    getExternalAPIParams,
  )
where

import Data.Maybe (fromMaybe)
import Data.Vector (toList)
import Lib.Config

data ExternalAPIParams = ExternalAPIParams
  { domainName :: String,
    apiKey :: String
  }

data LocationParams = LocationParams
  { latitude :: Double,
    longitude :: Double
  }

data WeatherParams = WeatherParams
  { locationParams :: LocationParams,
    timestamp :: Int
  }

data WeatherOffsets = WeatherOffsets
  { latitudeOffset :: Double,
    longitudeOffset :: Double,
    timestampOffset :: Int
  }

getAutoCacheLocationParams :: Config -> [LocationParams]
getAutoCacheLocationParams config =
  map
    ( \coord ->
        LocationParams
          (Lib.Config.latitude coord)
          (Lib.Config.longitude coord)
    )
    (toList $ autoCacheCoordinates config)

getExternalAPIParams :: Config -> IO ExternalAPIParams
getExternalAPIParams config = do
  apiKey' <- getApiKey
  let domainName' = fromMaybe "" (customExternalAPIDomain config)
  return
    ExternalAPIParams
      { domainName = domainName',
        apiKey = apiKey'
      }
