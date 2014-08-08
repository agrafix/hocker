{-# LANGUAGE TemplateHaskell #-}
module Docker.Types where

import Docker.Aeson
import Data.Aeson.TH

import qualified Data.Text as T

data Image
   = Image
   { i_id :: T.Text
   , i_created :: Int
   , i_size :: Int
   , i_virtualSize :: Int
   , i_repoTags :: [T.Text]
   } deriving (Show, Eq)

data PortMapping
   = PortMapping
   { pm_privatePort :: Int
   , pm_publicPort :: Int
   , pm_type :: T.Text
   } deriving (Show, Eq)

data Container
   = Container
   { c_id :: T.Text
   , c_image :: T.Text
   , c_command :: T.Text
   , c_created :: Int
   , c_status :: T.Text
   , c_ports :: [PortMapping]
   , c_sizeRw :: Int
   , c_sizeRootFs :: Int
   } deriving (Show, Eq)

$(deriveJSON (dockerOpts 2) ''Image)
$(deriveJSON (dockerOpts 2) ''Container)
$(deriveJSON (dockerOpts 3) ''PortMapping)
