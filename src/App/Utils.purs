module App.Utils where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Data.Argonaut.Core (JArray, Json, JObject, toArray, toObject)
import Data.Date (day, month)
import Data.DateTime (Date, DateTime(DateTime), Time(Time))
import Data.Either (Either(Right, Left))
import Data.Enum (fromEnum)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.StrMap (StrMap, lookup)
import Data.Tuple (Tuple(..))
import Debug.Trace (class DebugWarning, trace)

map_ :: forall m a b. Functor m => m b -> (b -> a) -> m a
map_ = flip map

infixr 5 Tuple as !


dateToString :: Date -> String
dateToString d =
  (segment $ fromEnum $ month d) <> "/" <> (segment $ fromEnum $ day d)

timeToString :: Time -> String
timeToString (Time h m _ _) =
  (segment $ fromEnum h) <> ":" <> (segment $ fromEnum m)

datetimeToString :: DateTime -> String
datetimeToString (DateTime date time) =
  dateToString date <> " " <> timeToString time

segment :: Int -> String
segment n =
  if n < 10 then "0" <> s else s
  where
    s = (toStringAs decimal n)

------------------------------------------------------

eitherToMaybe :: forall a e. Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right v) = Just v

maybeToEither :: ∀ a b. b -> Maybe a -> Either b a
maybeToEither l =
  maybe (Left l) Right

eitherOrError :: ∀ a s fx. Show s => Either s a -> Eff (console :: CONSOLE | fx) (Maybe a)
eitherOrError (Left msg) = errorShow msg *> pure Nothing
eitherOrError (Right val)  = pure $ Just val

lookup_ :: String -> StrMap Json -> Either String Json
lookup_ k o = maybeToEither ("Missing key " <> k) $ (lookup k o)

lookup' :: ∀ a. String -> (Json -> Maybe a) -> StrMap Json -> Either String a
lookup' k f = maybeToEither ("Missing key " <> k) <<< (f <=< lookup k)

toArray' :: Json -> Either String JArray
toArray' = maybeToEither "not an array" <<< toArray

toObject' :: Json -> Either String JObject
toObject' = maybeToEither "not an object" <<< toObject

unupdate :: ∀ f a. Newtype f a => (a -> a) -> (f -> f)
unupdate update =
  wrap <<< update <<< unwrap

trace' :: ∀ a. DebugWarning => String -> a -> a
trace' m = trace m <<< const
