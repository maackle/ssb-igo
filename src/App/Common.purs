module App.Common where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Spork.Html as H
import Ssb.Client (getClient)
import Ssb.Config (Config(..), SSB, ConfigData, defaultConfigData)

mainShs = "1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYRan/s="
testShs = "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A="

standardConfig :: ∀ fx. Eff ( ssb :: SSB | fx ) Config
standardConfig = do
  cfg <- defaultConfigData $ Nothing
  pure $ Config $ cfg { port = 8008, shs = mainShs }

devConfig :: String -> Int -> ∀ fx. Eff ( ssb :: SSB | fx ) Config
devConfig path port = do
  cfg <- defaultConfigData $ Just
            { path, keys: Nothing }
  pure $ Config $ cfg { port = port, shs = testShs }

getClient' = getClient =<< liftEff standardConfig

messageTypeString = "ssb-igo"


div_ :: ∀ a. String -> Array (H.Html a) -> H.Html a
div_ k = H.div [H.classes [k]]


class' = H.classes <<< Array.singleton
