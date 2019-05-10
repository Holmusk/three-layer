{-# LANGUAGE KindSignatures #-}

module Main where

import Elm (defaultSettings, generateElm)

import Lib.Core.Admin (Admin)
import Lib.Core.Id (Id)
import Lib.Core.User (User)

-- | Types from the library which should be exported to frontend
type ExportTypes =
    '[ Admin
     , User
     , Id ()
     ]

main :: IO ()
main = generateElm @ExportTypes $ defaultSettings "frontend/src/" ["Lib", "Core"]
