{-# LANGUAGE OverloadedStrings #-}
module Main where

import Docker.Client

main :: IO ()
main =
    do im <- listImages (RestHostPort "192.168.59.103" 2375)
       ct <- listContainers (RestHostPort "192.168.59.103" 2375) True
       print im
       print ct
