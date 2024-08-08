module Main (main) where

import Control.Concurrent (forkIO)
import Lib.Autocache
import Lib.Config (getApiKey)
import Lib.Server
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  apiKey <- getApiKey
  _ <- forkIO $ run 8081 $ app apiKey
  startAutoCaching
