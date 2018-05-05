module App.Common where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Ssb.Client (getClient)
import Ssb.Config (Config(..), SSB, ConfigData, defaultConfigData)

clientConfig :: ∀ fx. Eff ( ssb :: SSB | fx ) Config
clientConfig = do
  cfg <- defaultConfigData $ Just
            { path: "./ssb-data"
            , keys: Nothing }
  pure $ Config $ cfg { port = 8088, shs = "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=" }

getClient' = getClient =<< liftEff clientConfig

messageTypeString = "ssb-igo"
