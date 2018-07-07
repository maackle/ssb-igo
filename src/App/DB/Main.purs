module App.DB.Main where

import Prelude

import App.Flume (FlumeData, MapFn, ReduceFnImpl, decodeFlumeDb, encodeFlumeDb, initialDb, mapFn, reduceFn)
import App.Utils (trace')
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, info)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut (Json, jsonNull, toObject)
import Data.Argonaut.Generic.Argonaut (encodeJson)
import Data.Foreign (Foreign, toForeign)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..), fromJust, maybe')
import Data.StrMap as M
import Debug.Trace (spy, trace, traceA, traceAny, traceAnyA)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Ssb.Config (SSB)
import Ssb.Keys (loadOrCreateSync)
import Ssb.PullStream (PullStream)
import Ssb.Server (createFeed, createFeed', createFeedRemote', toPlugin)
import Ssb.Types (Sbot)

main :: Eff () Foreign
main = pure $ toForeign ssbIgoPlugin

ssbIgoPlugin =
  { init: \sbot -> unsafePerformEff $ init sbot
  , name: "ssbIgo"
  , version: "0.1"
  , manifest
  }
  where
    init sbot = do
      view <- flumeUse sbot "ssb-igo" flumeReducer
      pure { streamDb: (liveStream view) :: Eff _ PullStream
           , rawGet: _rawGet view  -- NOTE: the FFI here is hosed
           -- , rawTestFeed: rawTestFeed
           }
      where
          rawTestFeed path = unsafePerformEff do
            keys <- loadOrCreateSync path
            createFeedRemote' sbot keys

    manifest =
      { "streamDb": "source"
      , "rawGet": "async"
      -- , "rawTestFeed": "sync"
      }

flumeReducer :: FlumeReducer
flumeReducer = mkFlumeReducer1 "0.2" reducer mapFn codec initialDb
  where

    reducer =
      -- TODO: find a way to not encode and decode entire DB every time!!!
      mkFn2 \db msg ->
        let encoded = unsafePartial $ fromJust $ decodeFlumeDb db
        in encodeFlumeDb $ reduceFn encoded msg

    decode j = do
      dbJson <- M.lookup "value" =<< toObject j
      decodeFlumeDb dbJson
    encode j = unsafePartial $ fromJust $ do
      db <- M.lookup "value" =<< toObject j
      pure $ db # unsafeStringify

    resolve :: Maybe FlumeData -> FlumeData
    resolve m = maybe' (\_ -> unsafeCrashWith "cannot decode w/ flumeReducer codec") id m
    codec =
          { encode: encode
          , decode: decode >>> resolve
          }

type Codec a = {decode :: Json -> a, encode :: Json -> String}

foreign import data FlumeReducer :: Type
foreign import data FlumeView :: Type
foreign import mkFlumeReducer :: String -> ReduceFnImpl -> MapFn -> FlumeData -> FlumeReducer
foreign import mkFlumeReducer1 :: String -> ReduceFnImpl -> MapFn -> Codec FlumeData -> FlumeData -> FlumeReducer
foreign import flumeUse :: ∀ fx. Sbot -> String -> FlumeReducer -> Eff (ssb :: SSB | fx) FlumeView
foreign import liveStream :: ∀ fx. FlumeView -> Eff (ssb :: SSB | fx) PullStream
foreign import _rawGet :: FlumeView -> (FlumeData -> Unit)   -- NOTE: the FFI here is hosed
