module App.Streaming where

import Prelude

import App.IgoMsg (IgoMsg(OfferMatch), SsbMessage(SsbMessage), parseMessage)
import App.Utils (trace')
import Data.Argonaut (Json, toObject, toString)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Partial.Unsafe (unsafeCrashWith)


type FlumeDb = {}

type ReduceFn = FlumeDb -> Json -> FlumeDb
type ReduceFnImpl = Fn2 FlumeDb Json FlumeDb
type MapFn = Json -> Json

reduceFn :: ReduceFn
reduceFn db json =
  case msg of
    SsbMessage (OfferMatch terms) _ -> trace' "GOT IT!!" {}
    _ -> trace' "NOT QUITE :()" {}
  where
    msg = case parseMessage json of
      Right m -> m
      Left err -> unsafeCrashWith ("bad message: " <> err <> ". json = " <> show json)

mapFn :: MapFn
mapFn json = case messageType json of
  Just t -> if t == "igo" then json else unsafeCrashWith ("weird type " <> t)
  Nothing -> unsafeCrashWith "got msg with no type O_o (sync?)"


messageType :: Json -> Maybe String
messageType json = toObject json >>= lookup "value" >>= toObject >>= lookup "content" >>= toObject >>= lookup "type" >>= toString
