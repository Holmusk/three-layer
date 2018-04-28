{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Lib.Server.Search where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default
import           Lens.Micro
import           Lib.App
import qualified Proto.Foo                      as P
import           Protolude                      hiding ((&))
import           Servant
import           Servant.API.ContentTypes.Proto
import           Servant.Server

type SearchAPI = "api" :> "search" :> ReqBody '[Proto] P.SearchRequest :> Post '[Proto] P.SearchResponse

searchServer :: ServerT SearchAPI App
searchServer = handleSearch

handleSearch :: (MonadIO m) => P.SearchRequest -> m P.SearchResponse
handleSearch req =
  let
    searchResp =
      def & P.response .~ [show ((req ^. P.pageNumber) + 1)]
  in return searchResp
