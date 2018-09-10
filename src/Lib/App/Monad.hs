module Lib.App.Monad
       ( -- * Application monad
         App (..)
       , runAppAsHandler
       , runAppAsIO
       ) where

import Control.Exception (bracket)
import Control.Monad.Except (MonadError, throwError)
import Katip (ColorStrategy (ColorIfTerminal), Katip, KatipContext, KatipContextT,
              Severity (DebugS), Verbosity (V2), closeScribes, defaultScribeSettings, initLogEnv,
              mkHandleScribe, registerScribe, runKatipContextT)
import Servant.Server (Handler)

import Lib.App.Env (AppEnv)
import Lib.App.Error (AppError, toHttpError)

-- TODO: inject logger configuration directly
newtype App a = App
  { unApp :: KatipContextT (ReaderT AppEnv (ExceptT AppError IO)) a
  } deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError,
              MonadIO, Katip, KatipContext)

-- don't ask why; the only way to move logging into separate function and forget
-- about it
withLogger :: ((KatipContextT m a -> m a) -> IO b) -> IO b
withLogger unKatip = do
    -- TODO: make severity configurable
    handleScribe <- mkHandleScribe ColorIfTerminal stdout DebugS V2
    let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"

    bracket mkLogEnv closeScribes $ \logEnv -> do
        -- TODO: provide something meaningfull instead of initial context
        let initialContext = ()  -- this context will be attached to every log
                                 -- in your app and merged w/ subsequent contexts
        let initialNamespace = "app"

        unKatip (runKatipContextT logEnv initialContext initialNamespace)

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    res <- liftIO $ runAppAsIO env app
    either (\e -> print e >> throwError (toHttpError e)) pure res

-- | Helper for running route handlers in IO.
runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env app = withLogger $ \unKatip ->
    runExceptT $ usingReaderT env $ unKatip $ unApp app
