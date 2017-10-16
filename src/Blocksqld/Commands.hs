{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Commands where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Types.Header (Header)
import Network.HTTP.Client

import Blocksqld.Types
import Blocksqld.Database

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

getblockcount :: CommandM Int
getblockcount = undefined
  --let req = RpcRequest "getblockcount" [] ""

getblockhash' :: Int -> CommandM String
getblockhash' i = do
  let req = RpcRequest "getblockhash" [toJSON i] ""
  C.unpack <$> (sendRpcRequest req)

getblock' :: String -> CommandM Block
getblock' = undefined
  --let req = RpcRequest "getblockhash" [toJSON i] ""

getblockWithHeight :: Int -> CommandM Block
getblockWithHeight = getblockhash' >=> getblock'

getTXsFromBlockWithHeight :: Int -> CommandM [Tx]
getTXsFromBlockWithHeight = getblockhash' >=> getblock' >=> getTXsFromBlock

getTXsFromBlock :: Block -> CommandM [Tx]
getTXsFromBlock = undefined

sendRpcRequest :: RpcRequest -> CommandM BL.ByteString
sendRpcRequest rpc = do
  req  <- lift $ jsonRpcToHTTPRequest rpc
  resp <- sendHttpRequest req
  return (responseBody resp)

sendHttpRequest :: Request -> CommandM (Response BL.ByteString)
sendHttpRequest req = do
  e <- liftIO $ do
     mgr <- newManager defaultManagerSettings
     httpLbs req mgr
  return e

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

jsonRpcToHTTPRequest :: MonadThrow m => RpcRequest -> CoinHandler m Request
jsonRpcToHTTPRequest rpc = do
  host <- asks coinHost
  port <- asks coinPort
  u    <- asks coinRpcUser
  p    <- asks coinRpcPass
  let authheader = getAuthHeader u p
  initReq <- parseRequest $ "http://"++host++":"++(show port)
  return $ createJsonRpc initReq rpc authheader

