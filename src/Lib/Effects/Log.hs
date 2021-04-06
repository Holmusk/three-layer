-- | Logging action for the project. Currently just logs the output to terminal.

module Lib.Effects.Log
       ( mainLogAction

       , runAppAsHandler
       , runAppLogIO
       , runAppLogIO_
       ) where

import Colog (LogAction, Message, Msg (..), Severity, filterBySeverity, richMessageAction)
import Control.Monad.Except (liftEither)
import Servant.Server (Handler)

import Lib.App (App, AppEnv, AppError, runAppAsIO, toHttpError)


-- | Main log action for the application. Prints message with some metadata to @stdout@.
mainLogAction :: MonadIO m => Severity -> LogAction m Message
mainLogAction severity =
    filterBySeverity severity msgSeverity richMessageAction

----------------------------------------------------------------------------
-- Application runners with runners
----------------------------------------------------------------------------

-- | Runs application as servant 'Handler'.
runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    res <- liftIO $ runAppLogIO env app
    liftEither $ first toHttpError res

-- | Runs application like 'runAppAsIO' but also logs error.
runAppLogIO :: AppEnv -> App a -> IO (Either AppError a)
runAppLogIO env app = do
    appRes <- runAppAsIO env app
    logRes <- whenLeft (Right ()) appRes (logMPErrorIO env)
    pure $ appRes <* logRes

-- | Like 'runAppAsIO' but discards result.
runAppLogIO_ :: AppEnv -> App a -> IO ()
runAppLogIO_ env app = void $ runAppLogIO env app

----------------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------------

logMPErrorIO :: AppEnv -> AppError -> IO (Either AppError ())
logMPErrorIO env err = runAppAsIO env $ log E $ show err
