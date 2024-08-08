module Lib.Internal.Utils.Concurrency
  ( waitForFirstNonNothingResult,
  )
where

import Control.Concurrent (newEmptyMVar, takeMVar)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (tryPutMVar)

waitForFirstNonNothingResult ::
  IO (Maybe a) ->
  IO (Maybe a) ->
  IO (Maybe a)
waitForFirstNonNothingResult action1 action2 = do
  resultVar <- newEmptyMVar
  let setResult x = case x of
        Just y -> tryPutMVar resultVar (Just y)
        Nothing -> return False
  _ <-
    concurrently
      (action1 >>= setResult)
      (action2 >>= setResult)
  takeMVar resultVar
