-- | MonadReader wrappers around postgresql-simple library.

module Lib.Db.Functions
       ( WithDb
       , initialisePool

         -- * Sql functions
       , query
       , queryRaw
       , execute
       , executeRaw
       , executeMany
       , returning

         -- * Error handling
       , asSingleRow
       , singleRowError
       ) where

import Lib.App.Env (DbPool, Has, grab)
import Lib.App.Error (AppErrorType, WithError, dbError, throwOnNothingM)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql


-- | Constraint for monadic actions that wants access to database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

-- | Create 'Pool.Pool' by given credentials.
initialisePool :: ByteString -> IO DbPool
initialisePool credentials = Pool.createPool (Sql.connectPostgreSQL credentials) Sql.close 10 5 10

-- | Performs a query without arguments and returns the resulting rows.
queryRaw
    :: forall res env m .
       (WithDb env m, FromRow res)
    => Sql.Query
    -> m [res]
queryRaw q = withPool $ \conn -> Sql.query_ conn q
{-# INLINE queryRaw #-}

-- | Performs a query with arguments and returns the resulting rows with the
-- given parameters.
query
    :: forall res args env m .
       (WithDb env m, ToRow args, FromRow res)
    => Sql.Query
    -> args
    -> m [res]
query q args = withPool $ \conn -> Sql.query conn q args
{-# INLINE query #-}

-- | Executes a query without arguments that is not expected to return results.
executeRaw
    :: (WithDb env m)
    => Sql.Query
    -> m ()
executeRaw q = withPool $ \conn -> void $ Sql.execute_ conn q
{-# INLINE executeRaw #-}

-- | Executes a query with parameters that is not expected to return results.
execute
    :: forall args env m .
       (WithDb env m, ToRow args)
    => Sql.Query
    -> args
    -> m ()
execute q args = withPool $ \conn -> void $ Sql.execute conn q args
{-# INLINE execute #-}

-- | Executes a multi-row query that is not expected to return results.
executeMany
    :: (WithDb env m, ToRow args)
    => Sql.Query
    -> [args]
    -> m ()
executeMany q args = withPool $ \conn -> void $ Sql.executeMany conn q args
{-# INLINE executeMany #-}

returning
    :: (WithDb env m, ToRow args, FromRow res)
    => Sql.Query
    -> [args]
    -> m [res]
returning q args = withPool $ \conn -> Sql.returning conn q args
{-# INLINE returning #-}

-- | Perform action that needs database connection.
withPool :: WithDb env m => (Sql.Connection -> IO b) -> m b
withPool f = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool f
{-# INLINE withPool #-}

----------------------------------------------------------------------------
-- Error helpers
----------------------------------------------------------------------------

-- | Helper function working with results from a database when you expect
-- only one row to be returned.
asSingleRow :: (WithError m) => m [a] -> m a
asSingleRow res = withFrozenCallStack $ throwOnNothingM
    singleRowError
    (viaNonEmpty head <$> res)

singleRowError :: AppErrorType
singleRowError = dbError "Expected a single row, but got none"
