module Lib.App
       ( module Lib.App.Env
       , AppError (..)
       , Session (..)
       , App (..)
       , runAppAsHandler
       ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Logger (LoggingT, MonadLogger, runStdoutLoggingT)
import Servant.Server (Handler, err400, err401, err404, err500, errBody)

import Lib.App.Env
import Lib.App.Error (AppError (..))
import Lib.Effects.Session (MonadSession)
import Lib.Effects.User (MonadUser)

newtype App a = App {
    unApp :: LoggingT(ReaderT AppEnv (ExceptT AppError IO)) a
} deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError, MonadIO, MonadLogger)

instance MonadSession App
instance MonadUser App

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env action = do
  res <- liftIO $ runExceptT $ runReaderT (runStdoutLoggingT $ unApp action) env
  case res of
    Left (Invalid text)     -> throwError $ err400 { errBody = textToLBS text }
    Left NotFound           -> throwError err404
    Left (NotAllowed text)  -> throwError $ err401 { errBody = textToLBS text }
    Left (ServerError text) -> throwError $ err500 { errBody = textToLBS text }
    Right a                 -> return a
