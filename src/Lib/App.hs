module Lib.App
       ( module Lib.App.Env
       , AppError (..)
       , Session (..)
       , App (..)
       , runAppAsHandler
       ) where

import Control.Monad.Except (MonadError, throwError)
import Katip (ColorStrategy (ColorIfTerminal), Katip, KatipContext, KatipContextT,
              Severity (DebugS), Verbosity (V2), closeScribes, defaultScribeSettings, initLogEnv,
              mkHandleScribe, registerScribe, runKatipContextT)
import Servant.Server (Handler)

import Lib.App.Env
import Lib.App.Error (AppError (..), toHttpError)
import Lib.Effects.Session (MonadSession)

-- TODO: inject logger configuration directly
newtype App a = App
  { unApp :: KatipContextT (ReaderT AppEnv (ExceptT AppError IO)) a
  } deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError,
              MonadIO, Katip, KatipContext)

instance MonadSession App

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env action = do
  -- TODO: make severity configurable
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout DebugS V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"

  res <- liftIO $ bracket mkLogEnv closeScribes $ \logEnv -> do
    -- TODO: provide something meaningfull instead of initial context
    let initialContext = ()  -- this context will be attached to every log in your app and merged w/ subsequent contexts
    let initialNamespace = "app"

    runExceptT $ usingReaderT env
               $ runKatipContextT logEnv initialContext initialNamespace
               $ unApp action

  either (throwError . toHttpError) pure res
