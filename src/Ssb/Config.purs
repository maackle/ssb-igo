module Ssb.Config
  ( SSB
  , Config
  , Keys
  , Caps
  , defaultConfig
  )
where
import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Argonaut (class EncodeJson, Json, fromString, jsonEmptyObject, jsonNull, (:=), (~>))
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)


type Config =
  { path :: String
  , shs :: String
  , sign :: Maybe String
  , port :: Int
  , host :: String
  , keys :: Keys
  , manifest :: Maybe (StrMap String)
  }

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

defaultConfig :: ∀ f. DefaultConfigOpts -> Eff (ssb :: SSB | f) Config
defaultConfig Nothing = _defaultConfig jsonNull jsonNull
defaultConfig (Just {path, keys}) =
  case keys of
    Just k -> _defaultConfig (fromString path) (encodeJsonKeys k)
    Nothing -> _defaultConfig (fromString path) jsonNull

foreign import data SSB :: Effect
foreign import _defaultConfig :: ∀ f. Json -> Json -> Eff (ssb :: SSB | f) Config

-- defaultConfig = Config
--   { keys:
--   , caps: Nothing }
