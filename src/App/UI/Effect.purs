module App.UI.Effect where
import Prelude

import App.Common (getClient')
import App.IgoMsg (IgoMsg)
import App.IgoMsg as Msg
import App.UI.ClientQueries (devClient)
import App.UI.Model (DevIdentity)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..), maybe)
import Debug.Trace (traceAnyA)
import Ssb.Client (SbotConn, whoami)
import Ssb.Config (SSB)
import Ssb.Types (UserKey)

type AffSbot fx = (Aff (ssb :: SSB | fx) SbotConn)

data Effect a
  = Nofx a
  | Log String a
  | Publish (Maybe DevIdentity) IgoMsg a
  | GetIdentity (Maybe DevIdentity) ({id :: String} -> a)
  -- | PublishPrivate IgoMsg (Array UserKey) a

derive instance functorEffect ∷ Functor Effect

-- clientFromPath = maybe getClient' (\path -> testFeed path =<< getClient')

runEffect ∷ forall eff. Effect ~> Aff (dom ∷ DOM, ssb :: SSB, console :: CONSOLE | eff)
runEffect = case _ of
  Nofx next ->
    pure next
  Log msg next ->
    log ("Effect Log: " <> msg) *> pure next
  Publish ident payload next -> do
    sbot <- maybe getClient' devClient ident
    msg <- Msg.publishMsg' sbot payload
    traceAnyA msg
    pure next
  GetIdentity ident next -> do
    sbot <- maybe getClient' devClient ident
    who <- whoami sbot
    pure $ next who
  -- PublishPrivate msg recips next ->
  --   Msg.publishPrivateMsg msg recips *> pure next
