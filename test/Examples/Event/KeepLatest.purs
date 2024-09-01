module Test.Examples.Event.KeepLatest where

import Debug
import FRP.Event
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Control.Plus (empty)
import Data.Array (length, replicate, (..))
import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (oneOf, oneOfMap, sequence_)
import Data.Functor.Compose (Compose(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, under)
import Data.Op (Op(..))
import Data.Profunctor (lcmap)
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Traversable (foldr, for_, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (justNone, justOne, makeEvent, memoize, merge, subscribe)
import FRP.Event as Event
-- import FRP.Event (biSampleOn)
import FRP.Event.Class
import FRP.Event.Time (throttle, withTime)
import FRP.Poll as OptimizedPoll
import FRP.Poll.Unoptimized as UnoptimizedPoll
import Foreign.Object.ST as STObject
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

vocalpushE :: forall s. Show s => String -> (s -> Effect Unit) -> s -> Effect Unit
vocalpushE tag push value = do
  traceM $ tag <> ": " <> show value
  push value
  traceM "END"

vocalpush :: forall s. Show s => String -> (s -> ST Global Unit) -> s -> Effect Unit
vocalpush tag push value = do
  traceM $ tag <> ": " <> show value
  liftST $ push value
  traceM "END"

-- push channel outer         (Boolean) :        T         F
-- push channel when true     (Int)     : 1         3          5
-- push channel when false    (Int)     :     2         4          6
-- receive channel keepLatest (Int)     : 3              6

test_keepLatestEffectulKilling :: Effect Unit
test_keepLatestEffectulKilling = do
  eventAndPush_switcher <- liftST Event.create
  eventAndPush_channel_true <- liftST $ createPure
  eventAndPush_channel_false <- liftST $ createPure
  let
    (keepLatest_event :: Event Int) = keepLatest
      ( map
          ( \b ->
              if (spy "keepLatest GOT " b) then eventAndPush_channel_true.event
              else eventAndPush_channel_false.event
          )
          eventAndPush_switcher.event
      )
  keepLatest_event_unsubscribe <- subscribe keepLatest_event \k -> do
    log $ "subscriber GOT " <> show k
  vocalpush "PUSHING TO T" eventAndPush_channel_true.push 1
  vocalpush "PUSHING TO F" eventAndPush_channel_false.push 2
  vocalpushE "PUSHING TO S" eventAndPush_switcher.push true
  vocalpush "PUSHING TO T" eventAndPush_channel_true.push 3
  vocalpush "PUSHING TO F" eventAndPush_channel_false.push 4
  vocalpushE "PUSHING TO S" eventAndPush_switcher.push false
  vocalpush "PUSHING TO T" eventAndPush_channel_true.push 5
  vocalpush "PUSHING TO F" eventAndPush_channel_false.push 6

  -- TEST ENDED
  -- NOW LETS TEST KILLING

  traceM "keepLatest_event_unsubscribe" <> keepLatest_event_unsubscribe
  pure unit

test_keepLatest :: Effect Unit
test_keepLatest = do
  eventAndPush_switcher <- liftST $ createPure
  eventAndPush_channel_true <- liftST $ createPure
  eventAndPush_channel_false <- liftST $ createPure
  let
    (keepLatest_event :: Event Int) = keepLatest
      ( map
          ( \b ->
              if (spy "keepLatest GOT " b) then eventAndPush_channel_true.event
              else eventAndPush_channel_false.event
          )
          eventAndPush_switcher.event
      )
  keepLatest_event_unsubscribe <- subscribe keepLatest_event \k -> do
    log $ "subscriber GOT " <> show k
  vocalpush "PUSHING TO T" eventAndPush_channel_true.push 1
  vocalpush "PUSHING TO F" eventAndPush_channel_false.push 2
  vocalpush "PUSHING TO S" eventAndPush_switcher.push true
  vocalpush "PUSHING TO T" eventAndPush_channel_true.push 3
  vocalpush "PUSHING TO F" eventAndPush_channel_false.push 4
  vocalpush "PUSHING TO S" eventAndPush_switcher.push false
  vocalpush "PUSHING TO T" eventAndPush_channel_true.push 5
  vocalpush "PUSHING TO F" eventAndPush_channel_false.push 6
  traceM "keepLatest_event_unsubscribe" <> keepLatest_event_unsubscribe
  pure unit
  where
  vocalpush :: forall s. Show s => String -> (s -> ST Global Unit) -> s -> _ Unit
  vocalpush tag push value = do
    traceM $ tag <> ": " <> show value
    liftST $ push value
    traceM "END"

main :: Effect Unit
main = test_keepLatestEffectulKilling
