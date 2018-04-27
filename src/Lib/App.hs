module Lib.App(
  AppEnv(..),
  AppError(..),
  Session(..),
  App(..),
  runAppAsHandler,
) where

import           Control.Monad.Logger
import           Lib.App.Env
import           Lib.App.Error
import           Lib.Effects.Session
import           Lib.Effects.User
import           Protolude
import           Servant.Server

newtype App a = App {
    unApp :: LoggingT(ReaderT AppEnv (ExceptT AppError IO)) a
} deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError, MonadIO, MonadLogger)

instance MonadSession App
instance MonadUser App

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env action = do
  res <- liftIO $ runExceptT $ runReaderT (runStdoutLoggingT $ unApp action) env
  case res of
    Left (Invalid text)     -> throwError $ err400 { errBody = toSL text }
    Left NotFound           -> throwError err404
    Left (NotAllowed text)  -> throwError $ err401 { errBody = toSL text }
    Left (ServerError text) -> throwError $ err500 { errBody = toSL text }
    Right a                 -> return a
