module Extended.Control.Concurrent.Async
where

import Control.Concurrent.Async (waitEitherCatchCancel, withAsync)
import Control.Exception (SomeException)
import Control.Monad.Cont (ContT (..), runContT)
import Control.Monad.Trans (lift)

raceCatch
  :: IO a
  -> IO b
  -> IO (Either (Either SomeException a) (Either SomeException b))
raceCatch l r = flip runContT return $ do
  a <- ContT (withAsync l)
  b <- ContT (withAsync r)
  lift (waitEitherCatchCancel a b)

