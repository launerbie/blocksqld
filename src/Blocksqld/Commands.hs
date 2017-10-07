{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Commands where

import Data.Aeson
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Client


import Blocksqld.Types

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

setRPCRequest :: Request -> RpcRequest -> Request
setRPCRequest req r = req { method = "POST"
                          , requestHeaders = [contentType]
                          , requestBody = RequestBodyLBS $ encode $ toJSON $ r}

addHeader :: Request -> Header -> Request
addHeader req h = req { requestHeaders = requestHeaders req ++ [h] }


