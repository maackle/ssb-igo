module App.Common where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Ssb.Client (getClient)
import Ssb.Config (Config(..), SSB, ConfigData, defaultConfigData)

testShs = "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A="

standardConfig :: ∀ fx. Eff ( ssb :: SSB | fx ) Config
standardConfig = do
  cfg <- defaultConfigData $ Just
            { path: "./ssb-data"
            , keys: Nothing }
  pure $ Config $ cfg { port = 8088, shs = testShs }

devConfig :: String -> Int -> ∀ fx. Eff ( ssb :: SSB | fx ) Config
devConfig path port = do
  cfg <- defaultConfigData $ Just
            { path, keys: Nothing }
  pure $ Config $ cfg { port = port, shs = testShs }

getClient' = getClient =<< liftEff standardConfig

messageTypeString = "ssb-igo"
