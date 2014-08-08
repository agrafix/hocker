{-# LANGUAGE OverloadedStrings #-}
module Main where

import Docker.Client
import System.Environment
import qualified Data.Text as T

main :: IO ()
main =
    do dockerHostEnv <- getEnv "DOCKER_HOST"
       let dockerHost = parseDockerHost $ T.pack dockerHostEnv
       args <- getArgs
       case args of
         ["images"] -> runCmd $ listImages dockerHost
         ["containers"] -> runCmd $ listContainers dockerHost True
         _ ->
             do progName <- getProgName
                putStrLn $ "Usage: " ++ progName ++ " [command]"

runCmd :: Show a => (IO (Either String a)) -> IO ()
runCmd action =
    do mRes <- action
       case mRes of
         Left err -> error err
         Right t -> print t

parseDockerHost :: T.Text -> RestConn
parseDockerHost str =
    if T.length str < 6
    then error $ "Invalid DOCKER_HOST: " ++ (show str)
    else if T.take 6 str == "tcp://"
         then case T.breakOn ":" (T.drop 6 str) of
                ("", port) ->
                    RestHostPort "127.0.0.1" (readPort $ T.drop 1 port)
                (host, port) ->
                    RestHostPort (T.unpack host) (readPort $ T.drop 1 port)
         else if T.take 7 str == "unix://"
              then RestUnix (T.unpack $ T.drop 7 str)
              else error $ "Invalid DOCKER_HOST: " ++ (show str)
    where
      readPort p =
          let i :: Int
              i = read $ T.unpack p
          in fromIntegral i
