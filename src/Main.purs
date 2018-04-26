module Main where

import Prelude

import App.Action (Action(..))
import App.Effect (Effect(..), runEffect)
import App.IgoMsg as Msg
import App.Model (Model, initialModel)
import App.View (render)
import Control.Monad.Aff (Aff, Error)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error)
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
import Data.Array as Array
import Data.Const (Const)
import Data.Either (hush)
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
import Ssb.Client (SSB)


update ∷ Model -> Action -> App.Transition Effect Model Action
update model = case _ of
  Noop ->
    App.purely model
  PlaceStone ->
    let
      effects = App.lift (Publish (Msg.demoMsg) Noop)
    in
      { model, effects }

app ∷ App.App Effect (Const Void) Model Action
app =
  { render
  , update
  , subs: const mempty
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
handleException e = error $ show e

main ∷ Eff (FX) Unit
main = do

  let
    effectInterpreter :: ∀ i. Interpreter (Eff FX) Effect i
    effectInterpreter = (throughAff runEffect handleException)

    interpreter = (effectInterpreter `merge` never)

  inst ←
    App.makeWithSelector
      interpreter
      (app )
      "#app"
  inst.run

  void $ hashes \oldHash newHash ->
    F.for_ (routeAction newHash) \i -> do
      inst.push i
      inst.run
