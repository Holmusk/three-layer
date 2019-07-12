-- | MonadReader wrappers around @postgresql-simple@ library.

module Lib.Db.Functions
       ( WithDb
       , initialisePool

         -- * Sql functions
       , query
       , queryRaw
       , queryNamed
       , execute
       , executeRaw
       , executeMany
       , executeNamed
       , returning

         -- * Error handling
       , asSingleRow
       , singleRowError
       ) where

import PgNamed (NamedParam, PgNamedError)

import Lib.App.Env (DbPool, Has, grab)
import Lib.App.Error (AppErrorType, WithError, dbError, dbNamedError, throwError, throwOnNothingM)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import qualified PgNamed as Sql


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

-- | Performs a query with named parameters and returns a list of rows.
queryNamed
    :: (WithError m, WithDb env m, FromRow res)
    => Sql.Query
    -> [NamedParam]
    -> m [res]
queryNamed q params = withPool (\conn -> runExceptT $ Sql.queryNamed conn q params)
    >>= liftDbError
{-# INLINE queryNamed #-}

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

{- | Executes a query with named parameters, returning the
number of rows affected
-}
executeNamed
    :: (WithError m, WithDb env m)
    => Sql.Query
    -> [NamedParam]
    -> m Int64
executeNamed q params = withPool (\conn -> runExceptT $ Sql.executeNamed conn q params)
    >>= liftDbError
{-# INLINE executeNamed #-}

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

-- | Lift database named parameters errors.
liftDbError :: WithError m => Either PgNamedError a -> m a
liftDbError = either (throwError . dbNamedError) pure
{-# INLINE liftDbError #-}

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
