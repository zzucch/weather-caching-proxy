module Lib
  ( runServer,
  )
where

import Control.Concurrent (forkIO)
import Lib.Autocache (startAutoCaching)
import Lib.Server (app)
import Network.Wai.Handler.Warp (run)

runServer :: IO ()
runServer = do
  _ <- forkIO $ run 8081 app
  startAutoCaching
