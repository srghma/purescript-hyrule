module Test.Examples.Event.MakeEventMemoized where

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

test_makeEventE :: Effect Unit
test_makeEventE = do
  { event, unsubscribe } <- makeEventE \push -> do
    push 1
    push 2
    pure (log $ show "makeEventE unsubscribe is called")
  unsubscribeSubscriber1 <- subscribe event \k -> do
    log $ show "subscribe1" <> show k
  unsubscribeSubscriber2 <- subscribe event \k -> do
    log $ show "subscribe2" <> show k
  unsubscribeSubscriber1 -- will execute deleteObjHack
  unsubscribeSubscriber2
  unsubscribe -- TODO: better name is `stop`
  pure unit

---------------------------------------------------------------------

-- | output:
-- | create event2: start
-- | create event2: end
-- | create event2: start
-- | create event2: end
-- | trace t event1 value1
-- | subscribe1 1.0
-- | subscribe1 2.0
-- | trace t event1 value1
-- | subscribe2 1.0
-- | subscribe2 2.0
-- | trace t event1 value2
-- | subscribe1 1.0
-- | subscribe1 2.0
-- | trace t event1 value2
-- | subscribe2 1.0
-- | subscribe2 2.0
-- | unsubscribeSubscriber1
-- | stop event2
-- | trace t event1 value3
-- | subscribe2 1.0
-- | subscribe2 2.0
-- | unsubscribeSubscriber2
-- | stop event2
-- | pure unit

test_makeEvent :: Effect Unit
test_makeEvent = do
  { event, push } <- liftST $ createPure
  let
    (event2 :: Event Number) =
      makeEvent $ \giveMe_eventb_btoEventfulProgram_iGive_StSt -> do
        traceM "create event2: start"
        let
          interpret :: String -> Free (Compose (ST Global) (Tuple (Array Number))) Unit
          interpret t = trace ("trace t " <> unsafeCoerce t) $ \_ -> justMany [ 1.0, 2.0 ]
        stopInterpret <- giveMe_eventb_btoEventfulProgram_iGive_StSt event interpret
        traceM "create event2: end"
        pure do
          traceM "stop event2" <> stopInterpret
  unsubscribeSubscriber1 <- subscribe event2 \k -> do
    log $ "subscribe1 " <> show k
  unsubscribeSubscriber2 <- subscribe event2 \k -> do
    log $ "subscribe2 " <> show k
  liftST $ push "event1 value1"
  liftST $ push "event1 value2"
  traceM "unsubscribeSubscriber1" <> unsubscribeSubscriber1
  liftST $ push "event1 value3"
  traceM "unsubscribeSubscriber2" <> unsubscribeSubscriber2
  traceM "pure unit"
  pure unit

---------------------------------------------------------------------
-- | - create event2: start
-- | - create event2: end
-- | - trace t event1 value1
-- | - subscribe1 1.0
-- | - subscribe2 1.0
-- | - subscribe1 2.0
-- | - subscribe2 2.0
-- | - trace t event1 value2
-- | - subscribe1 1.0
-- | - subscribe2 1.0
-- | - subscribe1 2.0
-- | - subscribe2 2.0
-- | - unsubscribeSubscriber1
-- | - trace t event1 value3
-- | - subscribe2 1.0
-- | - subscribe2 2.0
-- | - unsubscribeSubscriber2
-- | - pure unit

test_makeEventMemoized :: Effect Unit
test_makeEventMemoized = do
  { event, push } <- liftST $ createPure
  let
    (event2 :: Event Number) =
      makeEvent $ \giveMe_eventb_btoEventfulProgram_iGive_StSt -> do
        traceM "create event2: start"
        let
          interpret :: String -> Free (Compose (ST Global) (Tuple (Array Number))) Unit
          interpret t = trace ("trace t " <> unsafeCoerce t) $ \_ -> justMany [ 1.0, 2.0 ]
        stopInterpret <- giveMe_eventb_btoEventfulProgram_iGive_StSt event interpret
        traceM "create event2: end"
        pure do
          traceM "stop event2" <> stopInterpret
  event2Memoized <- memoize event2
  unsubscribeSubscriber1 <- subscribe event2Memoized.event \k -> do
    log $ "subscribe1 " <> show k
  unsubscribeSubscriber2 <- subscribe event2Memoized.event \k -> do
    log $ "subscribe2 " <> show k
  liftST $ push "event1 value1"
  liftST $ push "event1 value2"
  traceM "unsubscribeSubscriber1" <> unsubscribeSubscriber1
  liftST $ push "event1 value3"
  traceM "unsubscribeSubscriber2" <> unsubscribeSubscriber2
  traceM "pure unit"
  pure unit

main = test_makeEventMemoized
