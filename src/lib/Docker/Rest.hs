{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Docker.Rest
    ( RestConn (..)
    , RestRequest (..)
    , sendRequest
    , module Network.HTTP.Types
    )
where

import Prelude hiding (take)
import Control.Exception
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Monoid
import Network.BSD
import Network.HTTP.Types
import Network.Socket
import System.IO.Streams.Network
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified System.IO.Streams as S

data RestConn
   = RestHostPort HostName PortNumber
   | RestUnix String

data RestRequest a
   = RestRequest
   { rr_conn :: RestConn
   , rr_method :: StdMethod
   , rr_path :: BS.ByteString
   , rr_postBody :: Maybe a
   }

sendRequest :: (FromJSON b, ToJSON a) => RestRequest a -> IO (Either String b)
sendRequest (RestRequest {..}) =
    do sock <-
           case rr_conn of
             RestHostPort hostname portnumber ->
                 do proto <- getProtocolNumber "tcp"
                    bracketOnError
                      (socket AF_INET Stream proto)
                      (sClose)
                      (\sock ->
                           do hostHdl <- getHostByName hostname
                              connect sock (SockAddrInet portnumber (hostAddress hostHdl))
                              return sock
                      )
             RestUnix sockStr ->
                 bracketOnError
                  (socket AF_UNIX Stream 0)
                  (sClose)
                  (\sock ->
                       do connect sock (SockAddrUnix sockStr)
                          return sock
                  )
       (is, os) <- socketToStreams sock
       let lineEnd = "\r\n"
           sendOut x = S.write (Just x) os
       sendOut $ renderStdMethod rr_method <> " " <> rr_path <> " HTTP/1.1" <> lineEnd
       case rr_postBody of
         Just postBody ->
             do let bs = BSL.toStrict $ encode postBody
                sendOut $ "Content-Type: application/json" <> lineEnd
                sendOut $ "Content-Length: " <> (BSC.pack $ show (BS.length bs)) <> lineEnd
                sendOut lineEnd
                sendOut lineEnd
                sendOut bs
         Nothing ->
             return ()
       sendOut $ lineEnd
       mResp <- S.read is
       sClose sock
       case mResp of
         Just resp ->
             do let breakMark = lineEnd <> lineEnd
                    (header, body') =
                        BS.breakSubstring breakMark resp
                    body = BS.drop (BS.length breakMark) body'
                    headerLines =
                        map fixHeaderLine $
                        BSC.split '\n' header
                case headerLines of
                  [] ->
                      return $ Left "Server didn't send any headers"
                  ( x : xs) ->
                      case BSC.split ' ' x of
                        [_httpVers, statusCode, _statusResp] ->
                            if statusCode == "200"
                            then processResp xs body
                            else return $ Left $ "Bad status code: " ++ show statusCode
                        _ ->
                            return $ Left $ "Invalid status line: " ++ show x

         Nothing ->
             return $ Left "Server didn't send a response!"
    where
      processResp xs body =
          do let headerMap = map (\bs ->
                                      let (k, v') = BSC.break (== ':') bs
                                      in (k, BS.drop 2 v')
                                 ) xs
                 isChunked =
                     case lookup "Transfer-Encoding" headerMap of
                       Just "chunked" -> True
                       _ -> False
             print body
             return $
                    if isChunked
                    then case parseOnly chunkP body of
                           Left err -> Left $ "Chunkparser failed: " ++ err
                           Right fullBs -> eitherDecodeStrict fullBs
                    else eitherDecodeStrict body

      fixHeaderLine bs =
          if BS.null bs
          then bs
          else let rev = BS.reverse bs
                   h = BSC.head rev
               in if h == '\r'
                  then BS.reverse $ BS.drop 1 rev
                  else bs

chunkP :: Parser BSC.ByteString
chunkP =
    chunkLoop
    where
      chunkLoop =
          do finished <- atEnd
             if finished
             then return BSC.empty
             else do chunkLen <- hexadecimal
                     endOfLine
                     bs <- take chunkLen
                     endOfLine
                     rest <- chunkLoop
                     return (bs <> rest)
