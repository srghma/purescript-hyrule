module FRP.Event.Time
  ( withTime
  , debounce
  , interval
  , delay
  ) where

import Prelude

import Data.Compactable (compact)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as Avar
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Timer (TimeoutId, clearInterval, setInterval, setTimeout)
import FRP.Event (Event, makeEventE, mapAccum)

-- | Create an event which reports the current time in milliseconds since the epoch.
withTime :: forall a. ({ value :: a, time :: Instant } -> Effect Unit) -> a -> Effect Unit
withTime f value = do
  time <- now
  f { time, value }

debounce :: forall a. Milliseconds -> Event { time :: Milliseconds, value :: a } -> Event { time :: Milliseconds, value :: a }
debounce period = compact <<< mapAccum go Nothing
  where
  go Nothing { time, value } = Tuple (Just time) (Just { time, value })
  go (Just lastTime) { time, value } =
    if unwrap time - unwrap lastTime > unwrap period then Tuple (Just time) (Just { time, value })
    else Tuple (Just lastTime) Nothing

-- | Create an event which fires every specified number of milliseconds.
interval' :: forall a. (Instant -> Effect a) -> Int -> Effect { event :: Event a, unsubscribe :: Effect Unit }
interval' f n = makeEventE \k -> do
  id <- setInterval n do
    time <- now
    f time >>= k
  pure (clearInterval id)

interval
  :: Int
  -> Effect
       { event :: Event Instant
       , unsubscribe :: Effect Unit
       }
interval = interval' pure

delay :: forall a. Int -> (Either TimeoutId (Tuple TimeoutId a) -> Effect Unit) -> a -> Effect Unit
delay n f value = launchAff_ do
  tid <- Avar.empty
  o <- liftEffect $ setTimeout n $ launchAff_ do
    t <- Avar.read tid
    liftEffect $ f (Right (Tuple t value))
  Avar.put o tid
  liftEffect $ f (Left o)
