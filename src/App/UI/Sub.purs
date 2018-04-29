module App.UI.Sub where

import Prelude hiding (sub)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (Json)
import Spork.EventQueue (EventQueueInstance)
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))
import Ssb.Config (SSB)

data Sub a = ReceiveSsbMessage (Json -> a)
derive instance functorSub :: Functor Sub

type SubEffects eff = (ssb :: SSB, console :: CONSOLE | eff)

interpreter ∷
  ∀ eff o  -- o is Action!!
  . ((Json -> Eff (SubEffects eff) Unit) -> Eff (SubEffects eff) Unit)
  -> Interpreter (Eff (SubEffects eff)) Sub o
interpreter setupListener = Interpreter $ EventQueue.withCont spec
  where
    listener :: EventQueueInstance (Eff (SubEffects eff)) o -> Sub o -> Json -> Eff (SubEffects eff) Unit
    listener queue sub json =
      case sub of
        ReceiveSsbMessage k -> queue.push $ (k json)

    spec :: EventQueueInstance (Eff (SubEffects eff)) o -> Sub o -> (Eff (SubEffects eff) Unit)
    spec queue sub = do
      log "SEtting up interpreter"
      setupListener $ listener queue sub
      pure unit
