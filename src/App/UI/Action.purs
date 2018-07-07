module App.UI.Action where

import Prelude

import App.Common (getClient')
import App.Flume (FlumeState(..), IndexedMatch(..), decodeFlumeDb, lastMoveKey, mapFn, myColor, reduceFn)
import App.IgoMsg (BoardPosition(..), GameTerms, IgoMove(..), IgoMsg(..), KibitzPayload, OfferMatchPayload)
import App.IgoMsg as Msg
import App.UI.ClientQueries (devClient)
import App.UI.Effect (Affect, Effect, runEffect)
import App.UI.Effect as E
import App.UI.Model (DevIdentity, Model, ezify)
import App.UI.Optics (ModelLens)
import App.UI.Optics as O
import App.UI.Routes (Route(..))
import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console as Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Eff
import DOM (DOM)
import DOM.Classy.Element (fromElement)
import DOM.Classy.HTMLElement (fromHTMLElement)
import DOM.Classy.Node (nodeValue)
import DOM.HTML.HTMLInputElement (setValue, value)
import DOM.Node.Types (Element)
import Data.Argonaut (Json, decodeJson, jsonNull)
import Data.Array (last)
import Data.Either (Either(Right, Left))
import Data.Lens (set)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.StrMap as M
import Debug.Trace (spy, traceA, traceAny, traceAnyA)
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Spork.App (lift, purely)
import Spork.App as App
import Spork.Html (ElementRef)
import Ssb.MessageTypes (AboutMessage(..))
import Ssb.Types (UserKey, MessageKey)
import Tenuki.Game (TenukiClient, setGameState)
import Tenuki.Game as Tenuki


data Action
  = Noop
  | SetRoute Route
  | UpdateFlume Json
  | UpdateFriends Json
  | UpdateIdentity {id :: UserKey}
  -- | UpdateScratch Event (String -> ScratchOffer)
  | CreateOffer OfferMatchPayload
  | Publish IgoMsg
  | SetDevIdentity (DevIdentity)

  | ManageRef String ElementRef
  | ManageTenukiGame IndexedMatch ElementRef
  | SetTenukiClient (Maybe TenukiClient)
  | SubmitKibitz MessageKey
  | UpdateModel (Model -> Model)
  | UpdateField' (String -> Either String (Model -> Model)) String

  | HandlePlayerAutocomplete (ModelLens String) String




publishFrom :: ∀ e. Maybe DevIdentity -> IgoMsg -> Msg.SA e Unit
publishFrom ident msg = do
  sbot <- maybe getClient' devClient ident
  Msg.publishMsg sbot msg

dispatchBoardMove :: ∀ e. Model -> IndexedMatch -> BoardPosition -> Msg.SA e Unit
dispatchBoardMove {devIdentity} match pos =
  let lastMove = lastMoveKey match
      msg = Msg.PlayMove
              { lastMove
              , move: PlayStone pos
              , subjectiveMoveNum: -1
              }
  in publishFrom devIdentity msg

dispatchToggleDead :: ∀ e. Model -> IndexedMatch -> BoardPosition -> Msg.SA e Unit
dispatchToggleDead {devIdentity} match pos =
  let lastMove = lastMoveKey match
      msg = Msg.PlayMove
              { lastMove
              , move: ToggleDead pos
              , subjectiveMoveNum: -1
              }
  in publishFrom devIdentity msg




update ∷ ∀ eff. Model -> Action -> App.Transition (Affect eff) Model Action
update model = case _ of
  Noop ->
    App.purely model
  SetRoute route ->
    App.purely model {route = route}
  UpdateIdentity {id} ->
    App.purely $ model { whoami = Just id }
  UpdateFlume json ->
    App.purely $ case model.flume, decodeFlumeDb json of
      FlumeFailure _, _ -> model
      _, Just db -> model { flume = FlumeDb db }
      FlumeUnloaded, Nothing -> model { flume = FlumeFailure "Flume index not intitialized"}
      FlumeDb flume, Nothing ->
        let mapped = mapFn json
        in if mapped == jsonNull
          then model
          else model { flume = FlumeDb $ reduceFn flume mapped }
  UpdateFriends json ->
    case decodeJson json :: Either String AboutMessage of
      Left reason -> {model, effects: lift $ pure Noop}
      Right (AboutMessage {content}) ->
        let
          name = content.name
          key = content.about
          user = M.lookup key model.userKeys # case _ of
            Nothing -> { name, key }
            Just u -> maybe {name, key} (\name -> u { name = Just name}) name

        in purely $ model
            { userKeys = M.insert key user model.userKeys
            , userNames = maybe model.userNames (\n -> M.insert n user model.userNames) name
            }
  -- UpdateScratch event f ->
  --   let val = target node
  --   in purely $ model { scratchOffer = f model.scratchOffer }

  Publish msg ->
    { model, effects: lift $ runEffect (publish msg) }

  CreateOffer payload ->
    let msg = OfferMatch payload
    in { model, effects: lift $ runEffect (publish msg)}
  -- DeclineOffer payload ->
  --   let msg = DeclineMatch payload
  --   in { model, effects: lift $ runEffect (publish msg)}
  SetDevIdentity ident ->
    { model: model { devIdentity = Just ident }
    , effects: lift $ runEffect (E.GetIdentity (Just ident) UpdateIdentity)
    }

  ManageTenukiGame match@(IndexedMatch {offerPayload, moves}) ref -> case ref of
    Created el ->
      let
        color = myColor match =<< model.whoami
        submitPlay pos =
          launchAff_ $ dispatchBoardMove model match $ wrap pos
        submitMarkDeadAt pos stones =
          launchAff_ $ dispatchToggleDead model match $ wrap pos
        callbacks =
          { submitPlay , submitMarkDeadAt }
        effects = lift $ liftEff $ SetTenukiClient <$> Just <$> do
            client <- Tenuki.createClient el offerPayload.terms color callbacks
            let steps = moves <#> (_.move <<< unwrap)
            setGameState (Tenuki.getGame client) steps
            pure client
      in {model, effects}
    Removed el ->
      {model, effects: lift $ pure $ SetTenukiClient Nothing }

  SubmitKibitz move -> maybe (purely model) id $ do
    el <- fromElement =<< M.lookup "kibitzInput" model.refs
    let pub = do
          text :: String <- liftEff $ value el
          liftEff $ setValue "" el
          runEffect $ publish $ Kibitz {move, text}
    pure { model: model { kibitzDraft = "" }
          , effects: lift pub
          }
  -- case M.lookup "kibitzInput" model.refs of
  --   Nothing -> purely model
  --   Just (el :: Element) ->
  --     let
  --       pub = do
  --         text :: String <- liftEff $  el
  --         runEffect $ publish $ Kibitz {move, text}
  --     in
  --       { model: model { kibitzDraft = "" }
  --       , effects: lift pub
  --       }

  SetTenukiClient client -> purely $ model { tenukiClient = client }

  ManageRef key ref -> case ref of
    Created el -> purely $ model { refs = M.insert key el model.refs}
    Removed el -> purely $ model { refs = M.delete key model.refs}

  UpdateModel f ->
    purely $ f model

  UpdateField' f val ->
    let
      effect = liftEff do
        case (f val) of
          -- Left err -> preventDefault event *> traceA err *> pure Noop
          Left err -> pure $ UpdateModel (set (O.scratchOffer <<< O.errorMsg) $ Just err)
          Right f -> pure $ UpdateModel f
    in { model, effects: lift effect}


  HandlePlayerAutocomplete lens val ->
    purely model

  where
    publish msg = E.Publish model.devIdentity msg Noop
