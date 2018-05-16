module App.UI.Effect where
import Prelude

import App.Common (getClient')
import App.IgoMsg (IgoMsg)
import App.IgoMsg as Msg
import App.UI.ClientQueries (testFeed)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Argonaut (Json)
import Data.Maybe (Maybe, maybe)
import Debug.Trace (traceAnyA)
import Ssb.Client (ClientConnection, whoami)
import Ssb.Config (SSB)
import Ssb.Types (UserKey)


data Effect a
  = Nofx a
  | Log String a
  | Publish (Maybe String) IgoMsg a
  | GetIdentity (Maybe String) ({id :: String} -> a)
  -- | PublishPrivate IgoMsg (Array UserKey) a

derive instance functorEffect ∷ Functor Effect

clientFromPath = maybe getClient' (\path -> testFeed path =<< getClient')

runEffect ∷ forall eff. Effect ~> Aff (dom ∷ DOM, ssb :: SSB, console :: CONSOLE | eff)
runEffect = case _ of
  Nofx next ->
    pure next
  Log msg next ->
    log ("Effect Log: " <> msg) *> pure next
  Publish feedPath msg next -> do
    feed <- clientFromPath feedPath
    traceAnyA feed
    Msg.publishMsg feed msg
    pure next
  GetIdentity feedPath next -> do
    feed <- clientFromPath feedPath
    who <- whoami feed
    pure $ next who
  -- PublishPrivate msg recips next ->
  --   Msg.publishPrivateMsg msg recips *> pure next
