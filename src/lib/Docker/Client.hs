{-# LANGUAGE OverloadedStrings #-}
module Docker.Client
    ( module Docker.Types
    , RestConn(..)
    , listImages
    , listContainers
    )
where

import Docker.Rest
import Docker.Types
import Data.Monoid

_API_VERS_ = "v1.13"

listImages :: RestConn -> IO (Either String [Image])
listImages rc =
    sendRequest req
    where
      req :: RestRequest ()
      req =
          RestRequest
          { rr_conn = rc
          , rr_method = GET
          , rr_path = "/" <> _API_VERS_ <> "/images/json"
          , rr_postBody = Nothing
          }

listContainers :: RestConn -> Bool -> IO (Either String [Container])
listContainers rc shouldShowAll =
    sendRequest req
    where
      showAll =
          if shouldShowAll
          then "1"
          else "0"
      req :: RestRequest ()
      req =
          RestRequest
          { rr_conn = rc
          , rr_method = GET
          , rr_path = "/" <> _API_VERS_ <> "/containers/json?size=1&all=" <> showAll
          , rr_postBody = Nothing
          }
