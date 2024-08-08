module Lib.Autocache
  ( startAutoCaching,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Lib.Config (getApiKey)
import Lib.Internal.DataFetching.DataFetch (getFromRemoteAndCacheData)

startAutoCaching :: IO ()
startAutoCaching = do
  apiKey <- getApiKey
  forever $ do
    let lat = 28.6448
    let lon = 77.216721
    response <- liftIO $ getFromRemoteAndCacheData apiKey lat lon
    print response
    threadDelay 60000000
