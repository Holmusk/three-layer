{-# LANGUAGE KindSignatures #-}

module Main where

import Elm (defaultSettings, generateElm)

import Lib.Core.Admin (Admin)
import Lib.Core.Id (Id)
import Lib.Core.User (User)
import Lib.Server.Auth (LoginRequest, LoginResponse)

-- | Types from the library which should be exported to frontend
type ExportTypes =
    '[ Admin
     , User
     , Id ()
     , LoginRequest
     ]

main :: IO ()
main = generateElm @ExportTypes $ defaultSettings "frontend/src/" ["Lib", "Core"]
