module App.UI.Sub where

import Prelude hiding (sub)

import App.Common (getClient')
import App.UI.ClientQueries (devClient, getStream)
import App.UI.Model (DevIdentity)
import Control.Monad.Aff (Fiber, error, joinFiber, killFiber, launchAff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, info)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..), maybe)
import Debug.Trace (traceA, traceAnyA)
import Spork.EventQueue (EventQueueInstance, EventQueueAccum)
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))
import Ssb.Client (ClientConnection)
import Ssb.Config (SSB)
import Ssb.PullStream (drain)

data Sub a = ReceiveSsbMessage (Maybe DevIdentity) (Json -> a)
derive instance functorSub :: Functor Sub

-- type SubEffects eff = (console :: CONSOLE | eff)

type FX fx = (ssb :: SSB, console :: CONSOLE | fx)
type E fx = Eff (FX fx)

-- NOTE: couldn't use stepper here because can't directly return an `Eff fx Action`
-- due to needing to defer to the listener to add items to the queue

-- NOTE: also couldn't use withCont because it executes every sub,
-- we can't differentiate between first time (for setup) and subsequent times

type SbotFiber eff = Fiber (FX eff) ClientConnection
type Handler eff = (Json -> E eff Boolean)
type SubState eff =
  { devIdentity :: Maybe DevIdentity
  , sbotFiber :: Maybe (SbotFiber eff)
  }

interpreter ∷
  ∀ eff o  -- o is Action!!
  . Interpreter (E eff) Sub o
interpreter = Interpreter $ EventQueue.withAccum spec
  where

    -- Receives a handler function which must be constructed
    -- inside the subscription interpreter, and hooks that up
    -- to a pull-stream drain() to fire Subs for every stream item
    listenWith :: Maybe DevIdentity -> Handler eff -> E eff (SbotFiber eff)
    listenWith ident fn = launchAff do
      client <- maybe getClient' devClient ident
      stream <- liftEff $ getStream client
      drain stream fn
      pure client

    spec :: EventQueueInstance (E eff) o -> E eff (EventQueueAccum (E eff) (SubState eff) (Sub o))
    spec queue = pure { init, update, commit }
      where
        getHandler :: (Json -> o) -> Handler eff
        getHandler k json = do
          traceA ("ReceiveSsbMessage: " <> (show json))
          queue.push (k json)
          queue.run
          pure true

        init :: SubState eff
        init =
          { devIdentity: Nothing
          , sbotFiber: Nothing
          }

        update m sub =
          case sub of
            ReceiveSsbMessage devIdentity k -> do
              case m.sbotFiber of
                Nothing -> do
                  fiber <- listenWith devIdentity $ getHandler k
                  pure $ {sbotFiber: Just fiber, devIdentity}
                Just fiber ->
                  if devIdentity /= m.devIdentity
                    then do
                      launchAff_ $ do
                        liftEff $ info "Cleaning up fiber: BEFORE"
                        killFiber (error "cleaning up draining client") fiber
                        liftEff $ info "Cleaning up fiber: AFTER"
                      fiber <- listenWith devIdentity $ getHandler k
                      pure $ {sbotFiber: Just fiber, devIdentity}
                    else
                      pure m

        commit = pure
