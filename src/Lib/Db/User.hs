{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @users@ table.

module Lib.Db.User
       ( getUserByEmail
       ) where

import Lib.App (WithError)
import Lib.Core.Email (Email)
import Lib.Core.User (User)
import Lib.Db.Functions (WithDb, asSingleRow, queryNamed)


getUserByEmail :: (WithDb env m, WithError m) => Email -> m User
getUserByEmail email = asSingleRow $ queryNamed [sql|
    SELECT id, email, name, pwd_hash
    FROM users
    WHERE email = LOWER(?email)
|] [ "email" =? email ]
