module ExampleSolve where
-- https://github.com/paf31/purescript-behaviors/issues/28#issue-277348392

import Data.Tuple
import Effect
import FRP.Event
import FRP.Event.Time as Time
import FRP.Poll
import Prelude

import Control.Monad.ST.Class (liftST)
import Effect.Console (log)

ε :: Number
ε = 0.001

second :: Poll
main :: Effect Unit
main = do
  -- | {event, push} <- liftST create
  -- | let pair = Tuple <$> event <*> event
  -- | _ <- subscribe pair (\x -> log ("Received: " <> show x))

  -- | let ⍺ = 1
  let exp = solve' 1.0 Time.second (ε * _)
  -- log "Pushing 1"
  -- log "Pushing 2"
