module App.UI.Routes where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match(..), end, lit, str)
import Ssb.Types (MessageKey)


data Route
  = NotFound String
  | Dashboard
  | ViewGame MessageKey


routes :: Match Route
routes = dashboard <|> viewGame <|> notFound
  where
    dashboard = Dashboard <$ lit "/"
    viewGame = ViewGame <$ lit "games" <*> str
    notFound = NotFound <$> str
