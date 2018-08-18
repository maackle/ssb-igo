module App.UI.Main (main) where

import Prelude

import App.UI.Action (Action(..))
import App.UI.Action as Action
import App.UI.Effect (runEffect)
import App.UI.Effect as E
import App.UI.Model (Model, initialModel)
import App.UI.Routes (routes)
import App.UI.Sub (Sub(..))
import App.UI.Sub as Sub
import App.UI.View as View
import Control.Monad.Aff (Aff, Error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error) as Eff
import Data.Array (singleton)
import Routing.Hash (matches)
import Spork.App as App
import Spork.Interpreter (Interpreter, basicAff, merge)
import Ssb.Config (SSB)


subs :: Model -> App.Batch Sub Action
subs model@{devIdentity} =
  App.batch $ identityFeeds <> games
  where
    -- NB: Need to hook in whoami to get proper names with UpdateAbout
    identityFeeds = singleton $ IdentityFeeds devIdentity {igoCb: UpdateFlume, friendsCb: UpdateAbout "TODO"}
    games = []
    -- games = case tenukiClient of
    --   Just game -> singleton $ MoveListener game (dispatchBoardMove model <<< wrap)
    --   _ -> []

app ∷ App.App (Aff FX) Sub Model Action
app =
  { render: View.render
  , update: Action.update
  , init: {model, effects}
  , subs
  }
  where
    model = initialModel
    effects = App.lift $ runEffect $ E.GetIdentity initialModel.devIdentity UpdateIdentity

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
  _ <- inst.run

  void $ matches routes \oldRoute route -> do
    _ <- inst.push $ SetRoute route
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
