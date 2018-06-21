module Tenuki.Game where

import Prelude

import App.IgoMsg (BoardPosition(..), BoardPositionData, GameTerms, IgoMove(..), IgoMsg, StoneColor(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM.Node.Types (Element)
import Data.Foldable (foldl, traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse)
import Debug.Trace (spy, trace)

type TenukiCallbacks a =
  { pass :: String -> Maybe a
  , resign :: String -> Maybe a
  , play :: String -> BoardPositionData -> Maybe a
  }

type TenukiTerms =
  { boardSize :: Int
  , handicapStones :: Int
  , komi :: Number
  }

type TenukiGameState =
  {  moveNumber :: Int
  ,  playedPoint :: Maybe BoardPositionData
  ,  color :: String
  ,  pass :: Boolean
  ,  blackPassStones :: Int
  ,  whitePassStones :: Int
  -- ,  intersections :: Int
  ,  blackStonesCaptured :: Int
  ,  whiteStonesCaptured :: Int
  ,  capturedPositions :: Array (Maybe BoardPositionData)
  -- ,  koPoint :: Int
  ,  boardSize :: Int
  }

type TenukiClientCallbacks e =
  { submitPlay :: BoardPositionData -> Eff e Unit
  , submitMarkDeadAt :: BoardPositionData -> Array {} -> Eff e Unit
  }

type PlayOpts = {render :: Boolean}

setGameState :: ∀ e. TenukiGame -> Array IgoMove -> Eff e Unit
setGameState game moves = traverse_ runMove (spy moves) *> render game where
  opts = {render: false}
  runMove = case _ of
    Pass -> playPass opts game
    PlayStone (BoardPosition pos) -> playAt opts pos game
    ToggleDead (BoardPosition pos) -> toggleDead opts pos game
    Resign -> trace "TODO: resign" $ const $ pure unit
    Finalize -> trace "TODO: finalize" $ const $ pure unit


createGame :: ∀ e. Element -> GameTerms -> Eff e TenukiGame
createGame el {handicap, size, komi} =
  _createGame el terms
  where terms = {handicapStones: handicap, boardSize: size, komi: komi}


createClient :: ∀ e. Element -> GameTerms -> Maybe StoneColor -> TenukiClientCallbacks e -> Eff e TenukiClient
createClient el {handicap, size, komi} color callbacks =
  _createClient el terms player callbacks
  where
    terms = {handicapStones: handicap, boardSize: size, komi: komi}
    player = case color of
      Just White -> "white"
      Just Black -> "black"
      Nothing -> "observer"


foreign import data TenukiGame :: Type
foreign import data TenukiClient :: Type
foreign import _createGame :: ∀ e. Element -> TenukiTerms -> Eff e TenukiGame
foreign import _createClient :: ∀ e. Element -> TenukiTerms -> String -> TenukiClientCallbacks e -> Eff e TenukiClient
foreign import setMoveCallback :: ∀ e. TenukiGame -> (BoardPositionData -> Eff e Unit) -> Eff e Unit
foreign import playAt :: ∀ e. PlayOpts -> BoardPositionData -> TenukiGame -> Eff e Unit
foreign import toggleDead :: ∀ e. PlayOpts -> BoardPositionData -> TenukiGame -> Eff e Unit
foreign import playPass :: ∀ e. PlayOpts -> TenukiGame -> Eff e Unit
foreign import render :: ∀ e. TenukiGame -> Eff e Unit
foreign import currentState :: ∀ e. TenukiGame -> TenukiGameState
foreign import getGame :: ∀ e. TenukiClient -> TenukiGame
foreign import getScore :: ∀ e. TenukiGame -> {black :: Int, white :: Int}
foreign import isOver :: ∀ e. TenukiGame -> Boolean
