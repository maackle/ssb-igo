module App.UI.Sub where

import Prelude hiding (sub)

import App.Common (getClient')
import App.UI.ClientQueries (devClient, getStream)
import App.UI.Model (DevIdentity)
import Control.Monad.Aff (Fiber, error, killFiber, launchAff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut (Json)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (traverse)
import Spork.EventQueue (EventQueueInstance, EventQueueAccum)
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..))
import Ssb.Config (SSB)
import Ssb.Friends (aboutStream)
import Ssb.PullStream (drainWith)
import Ssb.Server (messagesByType)
import Tenuki.Game (TenukiGame)

data Sub a
  = IdentityFeeds (Maybe DevIdentity) (SubCallbacks a)
  -- | MoveListener TenukiGame (BoardPositionData -> a)
derive instance functorSub :: Functor Sub

type SubCallbacks a = {igoCb :: (Json -> a), friendsCb :: (Json -> a)}

-- type SubEffects eff = (console :: CONSOLE | eff)

type FX fx = (ssb :: SSB | fx)
type E fx = Eff (FX fx)

-- NOTE: couldn't use stepper here because can't directly return an `Eff fx Action`
-- due to needing to defer to the listener to add items to the queue

-- NOTE: also couldn't use withCont because it executes every sub,
-- we can't differentiate between first time (for setup) and subsequent times

type EffFiber eff = E eff (Fiber (FX eff) Unit)
type Handler eff = (Json -> E eff Boolean)
type SubState eff =
  { devIdentity :: Maybe DevIdentity
  , sbotFibers :: Maybe (FiberArray eff)
  , tenukiGame :: Maybe TenukiGame
  }
type FiberArray eff = (Array (Fiber (FX eff) Unit))

interpreter ∷
  ∀ eff o  -- o is Action!!
  . Interpreter (E eff) Sub o
interpreter = Interpreter $ EventQueue.withAccum spec where

  spec :: EventQueueInstance (E eff) o -> E eff (EventQueueAccum (E eff) (SubState eff) (Sub o))
  spec queue = pure { init, update, commit } where

    flumeDbHandler :: (Json -> o) -> Handler eff
    flumeDbHandler k json = do
      queue.push (k json)
      queue.run
      pure true

    friendHandler :: (Json -> o) -> Handler eff
    friendHandler k json = do
      queue.push (k json)
      queue.run
      pure true

    flumeDbListener :: Maybe DevIdentity -> Handler eff -> EffFiber eff
    flumeDbListener ident fn = launchAff do
      client <- maybe getClient' devClient ident
      stream <- liftEff $ getStream client
      drainWith stream fn

    friendListener :: Maybe DevIdentity -> Handler eff -> EffFiber eff
    friendListener ident fn = launchAff do
      client <- maybe getClient' devClient ident
      stream <- liftEff $ aboutStream client
      drainWith stream fn

    setupListeners :: Maybe DevIdentity -> SubCallbacks o -> E eff (FiberArray eff)
    setupListeners devIdentity {igoCb, friendsCb} = do
      fiberFlume <- flumeDbListener devIdentity $ flumeDbHandler igoCb
      fiberFriends <- friendListener devIdentity $ friendHandler friendsCb
      pure [fiberFlume, fiberFriends]

    init :: SubState eff
    init =
      { devIdentity: Nothing
      , sbotFibers: Nothing
      , tenukiGame: Nothing
      }

    commit = pure

    update :: SubState eff -> Sub o -> E eff (SubState eff)
    update m sub =
      case sub of
        IdentityFeeds devIdentity callbacks -> do
          case m.sbotFibers of
            Nothing -> do
              fibers :: FiberArray eff <- setupListeners devIdentity callbacks
              pure m {sbotFibers = Just fibers, devIdentity = devIdentity}
            Just fibers ->
              if devIdentity /= m.devIdentity
                then do
                  launchAff_ $ traverse (killFiber (error "can't clean up the drainWith")) fibers
                  fibers' :: FiberArray eff <- setupListeners devIdentity callbacks
                  pure m {sbotFibers = Just fibers, devIdentity = devIdentity}
                else
                  pure m
        -- MoveListener game cb -> do
        --   maybe
        --     (pure unit)
        --     (flip setMoveCallback \_ -> pure unit)
        --     m.tenukiGame
        --   setMoveCallback game \pos -> do
        --     let action = cb pos
        --     queue.push action
        --     queue.run
        --   pure m { tenukiGame = Just game }
