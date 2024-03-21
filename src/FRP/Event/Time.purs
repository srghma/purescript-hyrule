module FRP.Event.Time
  ( withTime
  , debounce
  , debounceWith
  , interval
  ) where

import Prelude

import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Now (now)
import Effect.Timer (clearInterval, setInterval)
import FRP.Event (Event, makeEvent, makeEventE, subscribe)
import FRP.Event.Class (fix, gateBy)

-- | Create an event which reports the current time in milliseconds since the epoch.
withTime :: forall a. Event a -> Event { value :: a, time :: Instant }
withTime e = makeEvent \k ->
  subscribe e \value -> do
    time <- now
    k { time, value }

-- | On each event, ignore subsequent events for a given number of milliseconds.
debounce :: forall a. Milliseconds -> Event a -> Event a
debounce period = debounceWith (map { period, value: _ })

-- | Provided an input event and transformation, block the input event for the
-- | duration of the specified period on each output.
debounceWith
  :: forall a b
   . (Event a -> Event { period :: Milliseconds, value :: b })
  -> Event a
  -> Event b
debounceWith process event = map _.value $ fix \processed ->
  let
    expiries :: Event Instant
    expiries =
      map
        (\{ time, value } -> fromMaybe time (instant (unInstant time <> value)))
        (withTime (map _.period processed))

    comparison :: forall r. Maybe Instant -> { time :: Instant | r } -> Boolean
    comparison a b = maybe true (_ < b.time) a

    unblocked :: Event { time :: Instant, value :: a }
    unblocked = gateBy comparison expiries stamped
  in
    process (map _.value unblocked)
  where
  stamped :: Event { time :: Instant, value :: a }
  stamped = withTime event

-- | Create an event which fires every specified number of milliseconds.
interval :: Int -> Effect { event :: Event Instant, unsubscribe :: Effect Unit }
interval n = makeEventE \k -> do
  id <- setInterval n do
    time <- now
    k time
  pure (clearInterval id)