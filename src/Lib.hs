module Lib
  ( runServer,
  )
where

import Lib.Server (app)
import Network.Wai.Handler.Warp (run)

runServer :: IO ()
runServer = do
  run 8081 app
