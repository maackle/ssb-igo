module Tenuki.Game where

import Prelude

import App.IgoMsg (BoardPosition(..), IgoMove(..), IgoMsg)
import Control.Monad.Eff (Eff)
import DOM.Node.Types (Element)
import Data.Foldable (foldl)
import Data.Maybe (Maybe)

type TenukiCallbacks a =
  { pass :: String -> Maybe a
  , resign :: String -> Maybe a
  , play :: String -> TenukiMove -> Maybe a
  }

type TenukiMove = {x :: Int, y :: Int}
type PlayOpts = {render :: Boolean}

setGameState :: ∀ e. TenukiGame -> Array IgoMove -> Eff e TenukiGame
setGameState game = foldl f (pure game) where
  opts = {render: false}
  f game move =
    case move of
      Pass -> playPass opts =<< game
      Resign -> playResign opts =<< game
      PlayStone (BoardPosition pos) -> playAt opts pos =<< game

foreign import data TenukiGame :: Type
foreign import createGame :: ∀ e. Element -> Eff e TenukiGame
foreign import playPass :: ∀ e. PlayOpts -> TenukiGame -> Eff e TenukiGame
foreign import playResign :: ∀ e. PlayOpts -> TenukiGame -> Eff e TenukiGame
foreign import playAt :: ∀ e. PlayOpts -> TenukiMove -> TenukiGame -> Eff e TenukiGame
foreign import render :: ∀ e. TenukiGame -> Eff e TenukiGame
