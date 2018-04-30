module App.Common where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Ssb.Client (getClient)
import Ssb.Config (defaultConfig)

clientConfig = do
  cfg <- defaultConfig $ Just
            { path: "/Users/michael/.ssb-test"
            , keys: Nothing }
  pure cfg { port = 8088, shs = "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=" }

getClient' = getClient =<< liftEff clientConfig
