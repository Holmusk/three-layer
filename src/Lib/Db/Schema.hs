-- | Helper functions to create and drop database from @.sql@ files.

module Lib.Db.Schema
       ( prepareDb
       ) where

import Lib.Db.Functions (WithDb, executeRaw)


{- | Prepare data base for the testing environment:

1. Drop all existing tables.
2. Created tables from scratch.
3. Populate tables with test data.
-}
prepareDb :: (WithDb env m) => m ()
prepareDb = teardownDb >> setupDb

-- | Create tables from the @sql/schema.sql@ file and seed data.
setupDb :: (WithDb env m) => m ()
setupDb = executeFile "sql/schema.sql" >> executeFile "sql/seed.sql"

-- | Create tables from the @sql/schema.sql@ file.
teardownDb :: (WithDb env m) => m ()
teardownDb = executeFile "sql/drop.sql"

executeFile :: (WithDb env m) => FilePath -> m ()
executeFile path = do
    sqlStatements <- readFile path
    executeRaw (fromString sqlStatements)
