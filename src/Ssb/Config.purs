module Ssb.Config where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Argonaut (class EncodeJson, Json, fromString, jsonEmptyObject, jsonNull, (:=), (~>))
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.Record (insert)
import Data.StrMap (StrMap)
import Data.Symbol (SProxy(..))

data Config
  = Config ConfigData
  | TestConfig {|TestConfigRows}

type ConfigData =
  { path :: String
  , shs :: String
  , sign :: Maybe String
  , port :: Int
  , host :: String
  , keys :: Keys
  , manifest :: Maybe (StrMap String)
  }

type TestConfigRows =
  ( port :: Int
  , host :: String
  )

addTemp :: {|TestConfigRows} -> {temp :: Boolean | TestConfigRows}
addTemp cfg = insert (SProxy :: SProxy "temp") true cfg

type Keys =
  { id :: String
  , public :: String
  , private :: String
  }

encodeJsonKeys {id, public, private} =
  "id" := id
  ~> "public" := public
  ~> "private" := private
  ~> jsonEmptyObject

type Caps =
  { shs :: String
  , sign :: Maybe String
  }

type DefaultConfigOpts =
  Maybe { path :: String, keys :: Maybe Keys }

defaultConfigData :: ∀ fx. DefaultConfigOpts -> Eff (ssb :: SSB | fx) ConfigData
defaultConfigData Nothing = _defaultConfig jsonNull jsonNull
defaultConfigData (Just {path, keys}) =
  case keys of
    Just k -> _defaultConfig (fromString path) (encodeJsonKeys k)
    Nothing -> _defaultConfig (fromString path) jsonNull

foreign import data SSB :: Effect
foreign import _defaultConfig :: ∀ f. Json -> Json -> Eff (ssb :: SSB | f) ConfigData

-- defaultConfig = Config
--   { keys:
--   , caps: Nothing }
