module App.UI.Action where

import Prelude

import Data.Argonaut (Json)

data Action
  = Noop
  | PlaceStone
  | ReduceIgoMessage Json
