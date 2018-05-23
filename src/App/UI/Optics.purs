module App.UI.Optics where

import Prelude

import App.UI.Model (Model)
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

-- scratchOffer = prop (SProxy :: SProxy "scratchOffer")
type ModelLens a = Lens' Model a

scratchOffer =
  lens _.scratchOffer $ _ { scratchOffer = _ }
terms =
  lens _.terms $ _ { terms = _ }
myColor =
  lens _.myColor $ _ { myColor = _ }
opponent =
  lens _.opponent $ _ { opponent = _ }
name =
  lens _.name $ _ { name = _ }
key =
  lens _.key $ _ { key = _ }
