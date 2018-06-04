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
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Eff
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Classy.Event (currentTarget)
import DOM.Classy.Node (nodeValue)
import DOM.Event.Event (Event)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..), maybe)
import Debug.Trace (traceAnyA)
import Ssb.Client (whoami)
import Ssb.Config (SSB)
import Ssb.Types (UserKey, SbotConn)

type AffSbot fx = (Aff (ssb :: SSB | fx) SbotConn)

data Effect a
  = Nofx a
  | Log String a
  | Publish (Maybe DevIdentity) IgoMsg a
  | GetIdentity (Maybe DevIdentity) ({id :: String} -> a)
  | RawEffect (∀ f. Aff f a)
  | ReturnEventTarget Event (String -> a)

type Affect eff = Aff (dom ∷ DOM, ssb :: SSB, console :: CONSOLE | eff)

  -- | PublishPrivate IgoMsg (Array UserKey) a

-- derive instance functorEffect ∷ Functor Effect

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
    pure next
  GetIdentity ident next -> do
    sbot <- maybe getClient' devClient ident
    who <- whoami sbot
    pure $ next who
  ReturnEventTarget event next -> liftEff do
    val <- nodeValue $ currentTarget event
    Eff.log val
    pure $ next val
  RawEffect e -> e
  -- PublishPrivate msg recips next ->
  --   Msg.publishPrivateMsg msg recips *> pure next
