module App.UI.View where

import Prelude

import App.Flume (FlumeState(FlumeDb), IndexedRequest(IndexedRequest), encodeFlumeDb)
import App.UI.Action (Action)
import App.UI.Model (Model, EzModel, ezify)
import App.UI.Routes (Route(..))
import App.UI.View.Dashboard as View
import App.UI.View.Dev as Dev
import App.UI.View.Game as View
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Global.Unsafe (unsafeStringify)
import Spork.Html as H


render :: Model -> H.Html Action
render model =
  case ezify model of
    Nothing -> H.div [H.classes ["root-container"]]
      [ H.h1 [] [H.text "app loading..."]
      , H.code []
        [ H.text (unsafeStringify model)]
      ]
    Just ez ->
      H.div [H.classes ["root-container"]]
        -- Dev.devToolbar ez
        [ mainContent model ez
        -- , H.code []
        --   [ H.text (unsafeStringify model)]
        ]
  where
    showDb = case model.flume of
      FlumeDb d -> show $ encodeFlumeDb d
      d -> unsafeStringify d

mainContent :: Model -> EzModel -> H.Html Action
mainContent model ez@{db} = case model.route of
  Dashboard -> View.dashboard model ez
  ViewGame key ->
    let match = M.lookup key db.matches
    in View.viewGame model ez match
  NotFound path ->
    H.h1 [] [H.text $ "Not found: " <> path]

myRequest :: EzModel -> H.Html _
myRequest {db, whoami} =
  case r of
    Just request@(IndexedRequest {terms} {key}) ->
      requestLine request
    Nothing ->
      H.text "make a request?"
  where
    r = (M.values db.requests) # find \(IndexedRequest d {author}) ->
      author == whoami


requestLine :: IndexedRequest -> H.Html _
requestLine (IndexedRequest {terms} _) =
  H.li [] [H.text $ show terms.size]
