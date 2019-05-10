{-# LANGUAGE KindSignatures #-}

module Main where

import Elm (defaultSettings, generateElm)

import Lib.Core.Admin (Admin)
import Lib.Core.Email (Email)
import Lib.Core.Id (Id)
import Lib.Core.Jwt (JwtToken)
import Lib.Core.User (User)

-- | Types from the library which should be exported to frontend
type ExportTypes =
    '[ Admin
     , User
     , Id ()
     , Email
     , JwtToken
     ]

main :: IO ()
main = generateElm @ExportTypes $ defaultSettings "frontend/src/" ["Lib", "Core"]
