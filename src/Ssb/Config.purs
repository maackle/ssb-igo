module Ssb.Config where
import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Maybe (Maybe(..))


type ConfigCommon =
  ( path :: Maybe String
  , shs :: String
  , sign :: Maybe String
  , port :: Int
  , host :: String
  , keys :: Keys
  )

type Config =
  {
  | ConfigCommon
  }

-- type ClientConfig =
--   { | ConfigCommon
--   }

type Keys =
  { id :: String
  , public :: String
  , private :: String
  }

type Caps =
  { shs :: String
  , sign :: Maybe String
  }

defaultConfig :: ∀ f. Maybe String -> Eff (ssb :: SSB | f) Config
defaultConfig = _defaultConfig <<< encodeJson

foreign import data SSB :: Effect
foreign import _defaultConfig :: ∀ f. Json -> Eff (ssb :: SSB | f) Config

-- defaultConfig = Config
--   { keys:
--   , caps: Nothing }
