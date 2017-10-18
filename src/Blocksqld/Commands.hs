{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Commands where

import Control.Error
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Types.Header (Header)
import Network.HTTP.Client

import Blocksqld.Types
import Blocksqld.Database

type AuthHeader = Header
type TxHash = String
type BlockHash = String
type BlockHeader = String
type RawTx = String

runCommand = runReaderT . runMaybeT

getbestblockhash :: CommandM BlockHash
getbestblockhash = do
  let req = RpcRequest "getbestblockhash" [] ""
  rpcresp <- responseFromRpcRequest req
  let mString = parseMaybe parseJSON (rpcResult rpcresp)
  hoistMaybe mString

getblockcount :: CommandM Int
getblockcount = do
  let req = RpcRequest "getblockcount" [] ""
  rpcresp <- responseFromRpcRequest req
  let mInt = parseMaybe parseJSON (rpcResult rpcresp)
  hoistMaybe mInt

getblockhash :: Int -> CommandM String
getblockhash i = do
  let req = RpcRequest "getblockhash" [toJSON i] ""
  rpcresp <- responseFromRpcRequest req
  let mString = parseMaybe parseJSON (rpcResult rpcresp)
  hoistMaybe mString

getblockheader :: String -> CommandM BlockHeader
getblockheader = undefined

getblock :: String -> CommandM Block
getblock hash = do
  let req = RpcRequest "getblock" [toJSON hash] ""
  rpcresp  <- responseFromRpcRequest req
  let mString = parseMaybe parseJSON (rpcResult rpcresp)
  hoistMaybe mString

getblockWithHeight :: Int -> CommandM Block
getblockWithHeight = getblockhash >=> getblock

getTXsFromBlockWithHeight :: Int -> CommandM [Tx]
getTXsFromBlockWithHeight = getblockhash >=> getblock >=> getTXsFromBlock

getTXsFromBlock :: Block -> CommandM [Tx]
getTXsFromBlock = undefined

decoderawtransansaction :: String -> CommandM Tx
decoderawtransansaction = undefined

getrawtransaction :: String -> CommandM RawTx
getrawtransaction = undefined

------- JSON-RPC/HTTP -----------------
decodeHttpBodyToRpc :: BL.ByteString -> CommandM RpcResponse
decodeHttpBodyToRpc = hoistMaybe . decode

responseFromRpcRequest :: RpcRequest -> CommandM RpcResponse
responseFromRpcRequest = (sendRpcRequest >=> decodeHttpBodyToRpc)

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
