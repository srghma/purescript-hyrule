module Test.Examples.PollUnoptimized.Sample where

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
import FRP.Poll.Unoptimized (APoll(..))
import Foreign.Object.ST as STObject
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

test_test :: Effect Unit
test_test = do
  -- Create input events
  (eventAndPush_keyMaker :: { event :: Event Int, push :: Int -> ST Global Unit }) <- liftST createPure

  -- Step 2: Create a `Poll` using `step`, with an initial value of 0
  -- let poll = UnoptimizedPoll.step 0 eventAndPush_keyMaker.event

  let (pollUnwrapped :: Event (Int -> String) -> Event String) = \sampler -> Event.sampleOnRight eventAndPush_keyMaker.event sampler

  (eventAndPush_sampler :: { event :: Event (Int -> String), push :: (Int -> String) -> ST Global Unit }) <- liftST createPure

  let pollResultingEvent = pollUnwrapped eventAndPush_sampler.event

  pollResultingEvent_unsubscribe <- subscribe pollResultingEvent \v -> do
    log $ "pollResultingEvent_sampler GOT " <> show v

  liftST $ eventAndPush_sampler.push (\x -> "sampler1 func0 " <> show x) -- nothing
  liftST $ eventAndPush_keyMaker.push 0
  liftST $ eventAndPush_sampler.push (\x -> "sampler1 func1 " <> show x) -- yes
  liftST $ eventAndPush_sampler.push (\x -> "sampler1 func2 " <> show x) -- yes

  liftST $ eventAndPush_keyMaker.push 1
  liftST $ eventAndPush_sampler.push (\x -> "sampler1 func3 " <> show x) -- yes

test_sample_ :: Effect Unit
test_sample_ = do
  -- Create input events
  (eventAndPush_keyMaker :: { event :: Event Int, push :: Int -> ST Global Unit }) <- liftST createPure

  -- Step 2: Create a `Poll` using `step`, with an initial value of 0
  let poll = UnoptimizedPoll.step 0 eventAndPush_keyMaker.event

  let (pollUnwrapped :: Event (Int -> String) -> Event String) = let (APoll eventABtoEventB) = poll in eventABtoEventB

  (eventAndPush_sampler :: { event :: Event (Int -> String), push :: (Int -> String) -> ST Global Unit }) <- liftST createPure

  let pollResultingEvent = pollUnwrapped eventAndPush_sampler.event

  pollResultingEvent_unsubscribe <- subscribe pollResultingEvent \v -> do
    log $ "pollResultingEvent_sampler GOT " <> show v

  liftST $ eventAndPush_sampler.push (\x -> "sampler1 func1 " <> show x)
  liftST $ eventAndPush_sampler.push (\x -> "sampler1 func2 " <> show x)

  liftST $ eventAndPush_keyMaker.push 1
  liftST $ eventAndPush_sampler.push (\x -> "sampler1 func3 " <> show x)

  pure unit
  where
  vocalpush :: forall s. String -> (s -> ST Global Unit) -> s -> Effect Unit
  vocalpush tag push value = do
    traceM (tag <> ": " <> unsafeCoerce value)
    liftST $ push value
    traceM "END"

main :: Effect Unit
main = test_test
