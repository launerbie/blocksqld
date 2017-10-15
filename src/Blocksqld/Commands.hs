{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Commands where

import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Base64 as B64
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

-- use applyBasicAuth instead
getAuthHeader :: String -> String -> Header
getAuthHeader user pass  =
  let enc = (B64.encode . S8.pack)
      b   = enc (user++":"++pass)
  in ("Authorization", "Basic " `S8.append` b)

fromRpcToRequest ::  RpcRequest -> CoinHandler IO Request
fromRpcToRequest rpc = do
  host <- asks coinHost
  port <- asks coinPort
  u    <- asks coinRpcUser
  p    <- asks coinRpcPass
  let authheader = getAuthHeader u p
  initReq <- parseRequest $ "http://"++host++":"++(show port)
  return $ createJsonRpc initReq rpc authheader




