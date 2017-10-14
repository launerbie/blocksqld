{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Commands where

import Data.Aeson
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Client


import Blocksqld.Types

type AuthHeader = Header

getblockhash :: Int -> RpcRequest
getblockhash index = RpcRequest m p i
                     where m = "getblockhash"
                           p = [toJSON index]
                           i = "test"

getblock :: String -> RpcRequest
getblock h = RpcRequest m p i
             where m = "getblock"
                   p = [toJSON h]
                   i = "hash"

------- HTTP -----------------
contentType :: Header
contentType = ("content-type","text/plain")

createJsonRpc :: Request
              -> RpcRequest
              -> AuthHeader
              -> Request
createJsonRpc req rpc auth = req
  { method = "POST"
  , requestHeaders = [contentType, auth]
  , requestBody = RequestBodyLBS $ encode $ toJSON $ rpc
  }

addHeader :: Request -> Header -> Request
addHeader req h = req { requestHeaders = requestHeaders req ++ [h] }

