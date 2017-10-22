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

import Text.Pretty.Simple (pPrint)

import Blocksqld.Types
import Blocksqld.Database

type AuthHeader  = Header
type TxHash      = String
type BlockHash   = String
type RawTx       = String
type Hash        = String

runApp = runReaderT . runEitherT

getbestblockhash :: AppM BlockHash
getbestblockhash = getDecodedResponse req
  where req = RpcRequest "getbestblockhash" [] "1"

getblockcount :: AppM Int
getblockcount = getDecodedResponse req
  where req = RpcRequest "getblockcount" [] "2"

getblockhash :: Int -> AppM String
getblockhash i = getDecodedResponse req
  where req =  RpcRequest "getblockhash" [toJSON i] "3"

getblockheader :: Hash -> AppM BlockHeader
getblockheader h = getDecodedResponse req
  where req = RpcRequest "getblockheader" [toJSON h] "4"

getblock :: BlockHash -> AppM Block
getblock h = getDecodedResponse req
  where req = RpcRequest "getblock" [toJSON h] "5"

getrawtransaction :: TxHash -> AppM RawTx
getrawtransaction txid = getDecodedResponse req
  where req = RpcRequest "getrawtransaction" [toJSON txid] "6"

decoderawtransaction :: RawTx -> AppM Tx
decoderawtransaction rawtx = getDecodedResponse req
  where req = RpcRequest "decoderawtransaction" [toJSON rawtx] "7"

getblockWithHeight :: Int -> AppM Block
getblockWithHeight = getblockhash >=> getblock

getTXidsFromBlockWithHeight :: Int -> AppM [TxHash]
getTXidsFromBlockWithHeight = getblockhash >=> getblock >=> getTXidsFromBlock

getTXidsFromBlock :: Block -> AppM [TxHash]
getTXidsFromBlock b = return (blockTx b)

decoderaws :: [TxHash] -> AppM [Tx]
decoderaws txs = mapM (getrawtransaction >=> decoderawtransaction) txs

------- JSON-RPC/HTTP -----------------
getDecodedResponse :: FromJSON a => RpcRequest -> AppM a
getDecodedResponse = responseFromRpcRequest >=> parseRpcResult

decodeHttpBodyToRpc :: BL.ByteString -> AppM RpcResponse
decodeHttpBodyToRpc s = hoistEither (eitherDecode s)

responseFromRpcRequest :: RpcRequest -> AppM RpcResponse
responseFromRpcRequest = sendRpcRequest >=> decodeHttpBodyToRpc

parseRpcResult :: FromJSON a => RpcResponse -> AppM a
parseRpcResult resp = do
  let x = (parseEither parseJSON . rpcResult) resp
  case x of
    Left l -> do liftIO $ do print "parse error while parsing:"
                             pPrint resp
                             print l
                 hoistEither x
    Right r -> hoistEither x

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

