module FRP.Event.Time
  ( withTime
  , withDelay
  , debounce
  , interval'
  , interval
  ) where

import Prelude

import Data.Compactable (compact)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Op (Op(..))
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as Avar
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Timer (TimeoutId, clearInterval, setInterval, setTimeout)
import FRP.Event (Event, makeEventE, mapAccum)
import Safe.Coerce (coerce)

-- | Create an event which reports the current time in milliseconds since the epoch.
-- withTime :: forall a. Op (Effect Unit) { value :: a, time :: Instant } -> Op (Effect Unit) a

withTimeX value = withTime (\publishTimeValue -> ?a) value

withTime :: forall a. ( { time :: Instant
      , value :: a
      }
      -> Effect Unit
    )
    -> a -> Effect Unit
withTime = go
 -- withTime = (coerce :: (({ time :: Instant, value :: a } -> Effect Unit) -> a -> Effect Unit) -> _ -> Op (Effect Unit) a) go

  where
  go ::
    ( { time :: Instant
      , value :: a
      }
      -> Effect Unit
    )
    -> a -> Effect Unit
  go f value = do
    time <- now
    f { time, value }


withDelay :: forall a. Int -> (Either TimeoutId (Tuple TimeoutId a) -> (Effect Unit))  -> (a -> Effect Unit)
withDelay n = go
  where
  go f value = launchAff_ do
    tid <- Avar.empty
    o <- liftEffect $ setTimeout n $ launchAff_ do
      t <- Avar.read tid
      liftEffect $ f (Right (Tuple t value))
    Avar.put o tid
    liftEffect $ f (Left o)

-- | Create an event which fires every specifised number of milliseconds.
interval' :: forall a. ((a -> Effect Unit) -> (Instant -> Effect Unit)) -> Int -> Effect { event :: Event a, unsubscribe :: Effect Unit }
interval' f n = makeEventE \publisherOfA -> do
  id <- setInterval n do
    time <- now
    f publisherOfA time
  pure (clearInterval id)

intervalMap :: forall a. (Instant -> a) -> Int -> Effect { event :: Event a, unsubscribe :: Effect Unit }
intervalMap map = interval' (\publishA instant -> publishA (map instant))

interval
  :: Int
  -> Effect
       { event :: Event Instant
       , unsubscribe :: Effect Unit
       }
interval = interval' identity

debounce :: forall a. Milliseconds -> Event { time :: Instant, value :: a } -> Event { time :: Instant, value :: a }
debounce period = compact <<< mapAccum go Nothing
  where
  go Nothing { time, value } = Tuple (Just time) (Just { time, value })
  go (Just lastTime) { time, value } =
    if unwrap (unInstant time) - unwrap (unInstant lastTime) > unwrap period then Tuple (Just time) (Just { time, value })
    else Tuple (Just lastTime) Nothing
