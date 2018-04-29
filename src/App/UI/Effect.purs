module App.UI.Effect where
import Prelude

import App.IgoMsg (IgoMsg)
import App.IgoMsg as Msg
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Argonaut (Json)
import Ssb.Config (SSB)


data Effect a
  = Nofx a
  | Log String a
  | Publish IgoMsg a

derive instance functorEffect ∷ Functor Effect


runEffect ∷ forall eff. Effect ~> Aff (dom ∷ DOM, ssb :: SSB, console :: CONSOLE | eff)
runEffect = case _ of
  Nofx next -> pure next
  Log msg next -> log ("Effect Log: " <> msg) *> pure next
  Publish msg next -> do
    log "humm"
    Msg.publishMsg msg
    pure next
