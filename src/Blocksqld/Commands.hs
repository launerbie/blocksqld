{-# LANGUAGE OverloadedStrings   #-}
module Blocksqld.Commands where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
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

type AuthHeader  = Header
type TxHash      = String
type BlockHash   = String
type RawTx       = String
type Blockheader = String

runApp = runReaderT . runEitherT

getbestblockhash :: AppM BlockHash
getbestblockhash = do
  let req = RpcRequest "getbestblockhash" [] ""
  rpcresp <- responseFromRpcRequest req
  let mString = parseEither parseJSON (rpcResult rpcresp)
  hoistEither mString

getblockcount :: AppM Int
getblockcount = do
  let req = RpcRequest "getblockcount" [] ""
  rpcresp <- responseFromRpcRequest req
  let mInt = parseEither parseJSON (rpcResult rpcresp)
  hoistEither mInt

getblockhash :: Int -> AppM String
getblockhash i = do
  let req = RpcRequest "getblockhash" [toJSON i] ""
  rpcresp <- responseFromRpcRequest req
  let mString = parseEither parseJSON (rpcResult rpcresp)
  hoistEither mString

getblockheader :: String -> AppM Blockheader
getblockheader hash = do
  let req = RpcRequest "getblockheader" [toJSON hash] ""
  rpcresp  <- responseFromRpcRequest req
  let mBh = parseEither parseJSON (rpcResult rpcresp)
  hoistEither mBh

getblock :: String -> AppM Block
getblock hash = do
  let req = RpcRequest "getblock" [toJSON hash] ""
  rpcresp  <- responseFromRpcRequest req
  let mBlock = parseEither parseJSON (rpcResult rpcresp)
  hoistEither mBlock

getblockWithHeight :: Int -> AppM Block
getblockWithHeight = getblockhash >=> getblock

getTXidsFromBlockWithHeight :: Int -> AppM [TxHash]
getTXidsFromBlockWithHeight = getblockhash >=> getblock >=> getTXidsFromBlock

getTXidsFromBlock :: Block -> AppM [TxHash]
getTXidsFromBlock b = return (blockTx b)

getrawtransaction :: TxHash -> AppM RawTx
getrawtransaction txid = do
  let req = RpcRequest "getrawtransaction" [toJSON txid] ""
  rpcresp  <- responseFromRpcRequest req
  let mRawTx = parseEither parseJSON (rpcResult rpcresp)
  hoistEither mRawTx

decoderawtransansaction :: RawTx -> AppM Tx
decoderawtransansaction rawtx = do
  let req = RpcRequest "decoderawtransansaction" [toJSON rawtx] ""
  rpcresp  <- responseFromRpcRequest req
  let mTx = parseEither parseJSON (rpcResult rpcresp)
  hoistEither mTx

decoderaws :: [TxHash] -> AppM [Tx]
decoderaws txs = mapM (getrawtransaction >=> decoderawtransansaction) txs

------- JSON-RPC/HTTP -----------------
decodeHttpBodyToRpc :: BL.ByteString -> AppM RpcResponse
decodeHttpBodyToRpc s = hoistEither (eitherDecode s)

responseFromRpcRequest :: RpcRequest -> AppM RpcResponse
responseFromRpcRequest = (sendRpcRequest >=> decodeHttpBodyToRpc)

sendRpcRequest :: RpcRequest -> AppM BL.ByteString
sendRpcRequest rpc = do
  req  <- lift $ jsonRpcToHTTPRequest rpc
  resp <- lift $ sendHttpRequest req
  return (responseBody resp)

sendHttpRequest :: Request -> AppReaderT (Response BL.ByteString)
sendHttpRequest req = do
  mgr <- asks manager
  liftIO $ httpLbs req mgr

createJsonRpc :: Request -> RpcRequest -> AuthHeader -> Request
createJsonRpc req rpc auth = req
  { method = "POST"
  , requestHeaders = [("content-type","text/plain"), auth]
  , requestBody = RequestBodyLBS $ encode $ toJSON $ rpc
  }

-- use applyBasicAuth instead
getAuthHeader :: String -> String -> Header
getAuthHeader user pass  =
  let enc = (B64.encode . S8.pack)
      b   = enc (user++":"++pass)
  in ("Authorization", "Basic " `S8.append` b)

jsonRpcToHTTPRequest :: RpcRequest -> AppReaderT Request
jsonRpcToHTTPRequest rpc = do
  c <- asks coinConf
  let host = coinHost c
      port = coinPort c
      u    = coinRpcUser c
      p    = coinRpcPass c
  let authheader = getAuthHeader u p
  initReq <- parseRequest $ "http://"++host++":"++(show port)
  return $ createJsonRpc initReq rpc authheader

-- | there is also Data.List.Split.splitEvery
chunks :: (Num a) => Int -> [a] -> [[a]]
chunks s [] = []
chunks s xs = foo : chunks s bar
              where (foo, bar) = (splitAt s xs)

