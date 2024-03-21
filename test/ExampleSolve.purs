module ExampleSolve where
-- https://github.com/paf31/purescript-behaviors/issues/28#issue-277348392

import Data.Tuple
import Effect
import FRP.Event
import FRP.Poll
import Prelude

import Control.Monad.ST.Class (liftST)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Interval (Interval)
import Data.Op (Op(..))
import Data.Time (Millisecond)
import Data.Time.Duration (Seconds(..), toDuration)
import Effect.Console (log)
import FRP.Event.Time (withTime)
import FRP.Event.Time as FRP.Event.Time

ε :: Number
ε = 0.001

-- instant :: Poll Instant
-- instant = poll \e -> map (\{ value, time: t } -> value t) (withTime e)

-- -- | Get the current time in seconds since the epoch.
-- seconds :: Poll Seconds
-- seconds = map (toDuration <<< unInstant) instant

-- -- second :: Poll Instant
-- -- second = sham (FRP.Event.Time.interval 1000)

-- main :: Effect Unit
-- main = do
--   -- | {event, push} <- liftST create
--   -- | let pair = Tuple <$> event <*> event
--   -- | _ <- subscribe pair (\x -> log ("Received: " <> show x))

--   -- | let ⍺ = 1
--   let exp = solve' 1.0 seconds (ε * _)
--   log $ exp
--   -- log "Pushing 1"
--   -- log "Pushing 2"
