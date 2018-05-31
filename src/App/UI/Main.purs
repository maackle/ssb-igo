module App.UI.Main where

import Prelude

import App.Common (getClient')
import App.IgoMsg as Msg
import App.UI.Action (Action(..))
import App.UI.Action as Action
import App.UI.Effect (Effect(..), Affect, runEffect)
import App.UI.Model (Model, initialModel)
import App.UI.Routes (routes)
import App.UI.Sub (Sub(..), Handler)
import App.UI.Sub as Sub
import App.UI.View as View
import Control.Monad.Aff (Aff, Error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error) as Eff
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
import Routing.Hash (hashes, matches)
import Simple.JSON (readJSON, writeJSON)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (Interpreter(..), basicAff, liftNat, merge, never, throughAff)
import Ssb.Config (SSB)



subs :: Model -> App.Batch Sub Action
subs {devIdentity} =
  App.lift $ IdentityFeeds devIdentity {igoCb: UpdateFlume, friendsCb: UpdateFriends}

app ∷ App.App (Aff FX) Sub Model Action
app =
  { render: View.render
  , update: Action.update
  , init: {model, effects}
  , subs
  }
  where
    model = initialModel
    effects = App.lift $ runEffect $ GetIdentity initialModel.devIdentity UpdateIdentity

type FX = App.AppEffects (ssb :: SSB, console :: Eff.CONSOLE)

handleException :: ∀ f. Error -> Eff (console :: Eff.CONSOLE | f) Unit
handleException e = Eff.error $ show e

main ∷ Eff (FX) Unit
main = do

  inst <-
    App.makeWithSelector
      (effectInterpreter `merge` Sub.interpreter)
      (app )
      "#app"
  inst.run

  void $ matches routes \oldRoute route -> do
    inst.push $ SetRoute route
    inst.run

  -- runAff_ (const $ pure unit) $ do
  --   {id} <- whoami =<< getClient'
  --   liftEff $ inst.push (InitState {id})
  --   liftEff inst.run

  where

    effectInterpreter :: ∀ i. Interpreter (Eff FX) (Aff FX) i
    effectInterpreter = basicAff handleException
    -- effectInterpreter :: ∀ i. Interpreter (Eff FX) Effect i
    -- effectInterpreter = (throughAff runEffect handleException)
    -- effectInterpreterEff = liftNat runEffect
