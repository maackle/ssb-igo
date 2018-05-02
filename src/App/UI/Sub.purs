module App.UI.Sub where

import Prelude hiding (sub)

import Control.Monad.Eff (Eff)
import Data.Argonaut (Json)
import Debug.Trace (traceA, traceAnyA)
import Spork.EventQueue (EventQueueInstance, EventQueueAccum)
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))

data Sub a = ReceiveSsbMessage (Json -> a)
derive instance functorSub :: Functor Sub

-- type SubEffects eff = (console :: CONSOLE | eff)

type E fx = Eff ( fx)

-- NOTE: couldn't use stepper here because can't directly return an `Eff fx Action`
-- due to needing to defer to the listener to add items to the queue

-- NOTE: also couldn't use withCont because it executes every sub,
-- we can't differentiate between first time (for setup) and subsequent times

type Handler eff = (Json -> E eff Boolean)

interpreter ∷
  ∀ eff o  -- o is Action!!
  . (Handler eff -> E eff Unit)
  -> Interpreter (E eff) Sub o
interpreter listenWith = Interpreter $ EventQueue.withAccum spec
  where
    spec :: EventQueueInstance (E eff) o -> E eff (EventQueueAccum (E eff) Boolean (Sub o))
    spec queue = pure { init, update, commit }
      where
        getHandler :: Sub o -> Handler eff
        getHandler sub json =
          case sub of
            ReceiveSsbMessage k -> do
              traceA ("ReceiveSsbMessage: " <> (show json))
              queue.push (k json)
              queue.run
              pure true

        init = false

        update started sub = do
          when (not started) $ listenWith $ getHandler sub
          pure true

        commit = pure
