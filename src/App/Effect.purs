module App.Effect where
import Prelude

import App.IgoMsg (IgoMsg)
import App.IgoMsg as Msg
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Argonaut (Json)
import Ssb.Client (SSB)


data Effect a
  = Nofx a
  | Publish IgoMsg a

derive instance functorEffect ∷ Functor Effect


runEffect ∷ forall eff. Effect ~> Aff (dom ∷ DOM, ssb :: SSB, console :: CONSOLE | eff)
runEffect = case _ of
  Nofx next -> pure next
  Publish msg next -> do
    log "humm"
    Msg.publishMsg msg
    pure next
