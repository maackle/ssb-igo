module Ssb.Friends where


import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Data.Foreign (Foreign, toForeign)
import Data.StrMap (StrMap)
import Ssb.Common (SE)
import Ssb.PullStream (PullStream)
import Ssb.Types (Sbot, SbotConn, UserKey)


createFriendStream :: ∀ fx. SbotConn -> {} -> SE fx PullStream
createFriendStream sbot opts = _createFriendStream sbot $ toForeign opts

hops :: forall fx. Sbot -> String -> Aff fx (StrMap Int)
hops sbot = fromEffFnAff <<< _hops1 sbot

foreign import _createFriendStream :: ∀ fx. Sbot -> Foreign -> SE fx PullStream
foreign import _hops1 :: ∀ fx. Sbot -> UserKey -> EffFnAff fx (StrMap Int)
