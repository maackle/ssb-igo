module App.UI.Optics where

import Prelude

import App.UI.Model (Model)
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

-- scratchOffer = prop (SProxy :: SProxy "scratchOffer")
type ModelLens a = Lens' Model a

named name = prop (SProxy :: SProxy "size")

scratchOffer =
  lens _.scratchOffer $ _ { scratchOffer = _ }
terms =
  lens _.terms $ _ { terms = _ }
myColor =
  lens _.myColor $ _ { myColor = _ }
opponent =
  lens _.opponent $ _ { opponent = _ }
name = prop (SProxy :: SProxy "name")
key = prop (SProxy :: SProxy "key")
size = prop (SProxy :: SProxy "size")
komi = prop (SProxy :: SProxy "komi")
handicap = prop (SProxy :: SProxy "handicap")
errorMsg = prop (SProxy :: SProxy "errorMsg")
kibitzDraft = prop (SProxy :: SProxy "kibitzDraft")
