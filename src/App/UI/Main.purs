module App.UI.Main where

import Prelude

import App.Common (getClient')
import App.IgoMsg as Msg
import App.UI.Action (Action(..))
import App.UI.ClientQueries (getStream)
import App.UI.Effect (Effect(..), runEffect)
import App.UI.Model (Model, initialModel)
import App.UI.Sub (Sub(..), Handler)
import App.UI.Sub as Sub
import App.UI.View (render)
import Control.Monad.Aff (Aff, Error, launchAff_)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log) as Eff
import Control.Monad.Eff.Timer (TIMER, setInterval)
import Control.MonadZero (guard)
import DOM (DOM)
import DOM.Classy.Element (fromElement) as DOM
import DOM.Event.KeyboardEvent (key) as DOM
import DOM.Event.Types (KeyboardEvent) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.HTMLElement (focus) as DOM
import DOM.HTML.Window (localStorage) as DOM
import DOM.Node.Types (Element) as DOM
import DOM.WebStorage.Storage (getItem, setItem) as DOM
import Data.Argonaut (Json, jsonNull)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable as F
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Routing.Hash (hashes)
import Simple.JSON (readJSON, writeJSON)
import Spork.App as App
import Spork.Html as H
import Spork.Html.Elements.Keyed as K
import Spork.Interpreter (Interpreter(..), liftNat, merge, never, throughAff)
import Ssb.Client (getClient)
import Ssb.Config (SSB)
import Ssb.PullStream (PullStream, drain)


update ∷ Model -> Action -> App.Transition Effect Model Action
update model = case _ of
  Noop ->
    App.purely model
  PlaceStone ->
    let
      effects = App.lift (Publish (Msg.demoMsg) Noop)
    in
      { model, effects }
  ReduceIgoMessage json ->
    { model, effects: App.lift (Log "hahaha" Noop) }

subs :: Model -> App.Batch Sub Action
subs _ =
  App.lift $ ReceiveSsbMessage ReduceIgoMessage

app ∷ App.App Effect Sub Model Action
app =
  { render
  , update
  , subs
  , init: App.purely model
  }
  where
    model = initialModel


routeAction ∷ String -> Maybe Action
routeAction = case _ of
  "/"         -> Nothing
  _           -> Nothing

type FX = App.AppEffects (ssb :: SSB, console :: CONSOLE)

handleException :: ∀ f. Error -> Eff (console :: CONSOLE | f) Unit
handleException e = Eff.error $ show e

main ∷ Eff (FX) Unit
main = do

  inst <-
    App.makeWithSelector
      (effectInterpreter `merge` Sub.interpreter listenWith)
      (app )
      "#app"
  inst.run

  void $ hashes \oldHash newHash ->
    F.for_ (routeAction newHash) \i -> do
      inst.push i
      inst.run

  where
    effectInterpreter :: ∀ i. Interpreter (Eff FX) Effect i
    effectInterpreter = (throughAff runEffect handleException)
    -- effectInterpreterEff = liftNat runEffect


    -- Receives a handler function which must be contructed
    -- inside the subscription interpreter, and hooks that up
    -- to a pull-stream drain() to fire Subs for every stream item
    listenWith :: Handler FX -> Eff FX Unit
    listenWith fn = launchAff_ do
      client <- getClient'
      stream <- liftEff $ getStream client
      drain stream fn
      pure unit
