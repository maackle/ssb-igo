module Tenuki.Game where

import Prelude

import App.IgoMsg (BoardPosition(..), GameTerms, IgoMove(..), IgoMsg, BoardPositionData)
import Control.Monad.Eff (Eff)
import DOM.Node.Types (Element)
import Data.Foldable (foldl, traverse_)
import Data.Maybe (Maybe)
import Data.Traversable (for, traverse)

type TenukiCallbacks a =
  { pass :: String -> Maybe a
  , resign :: String -> Maybe a
  , play :: String -> BoardPositionData -> Maybe a
  }

type TenukiTerms = {boardSize :: Int, handicapStones :: Int, komi :: Number}
type PlayOpts = {render :: Boolean}

setGameState :: ∀ e. TenukiGame -> Array IgoMove -> Eff e Unit
setGameState game moves = traverse_ runMove moves *> render game where
  opts = {render: false}
  runMove = case _ of
    Pass -> playPass opts game
    Resign -> playResign opts game
    PlayStone (BoardPosition pos) -> playAt opts pos game


createGame :: ∀ e. Element -> GameTerms -> Eff e TenukiGame
createGame el {handicap, size, komi} =
  _createGame el {handicapStones: handicap, boardSize: size, komi: komi}


foreign import data TenukiGame :: Type
foreign import _createGame :: ∀ e. Element -> TenukiTerms -> Eff e TenukiGame
foreign import setMoveCallback :: ∀ e. TenukiGame -> (BoardPositionData -> Eff e Unit) -> Eff e Unit
foreign import playPass :: ∀ e. PlayOpts -> TenukiGame -> Eff e Unit
foreign import playResign :: ∀ e. PlayOpts -> TenukiGame -> Eff e Unit
foreign import playAt :: ∀ e. PlayOpts -> BoardPositionData -> TenukiGame -> Eff e Unit
foreign import render :: ∀ e. TenukiGame -> Eff e Unit
