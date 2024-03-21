module Example2 where
-- https://github.com/paf31/purescript-behaviors/issues/28#issue-277348392
import Data.Tuple
import Effect
import FRP.Event
import Prelude

import Control.Monad.ST.Class (liftST)
import Effect.Console (log)

main :: Effect Unit
main = do
  {event, push} <- liftST create
  let pair = Tuple <$> event <*> event
  _ <- liftST $ subscribe pair (\x -> log ("Received: " <> show x))
  log "Pushing 1"
  push 1
  log "Pushing 2"
  push 2
