module Lib.Core.Types exposing (..)

import Time exposing (Posix)


type alias Admin =
    { id : Id
    , email : String
    , hash : String
    }

type alias User =
    { id : Id
    , name : String
    , email : String
    , hash : String
    }

type alias Id =
    { unId : String
    }




