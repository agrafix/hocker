module Docker.Aeson where

import qualified Data.Char as Char

import Data.Aeson
import Data.Aeson.TH

dockerOpts i =
    defaultOptions
    { fieldLabelModifier = ucFirst . drop i }
    where
      ucFirst (x : xs) = Char.toUpper x : xs
      ucFirst [] = []
