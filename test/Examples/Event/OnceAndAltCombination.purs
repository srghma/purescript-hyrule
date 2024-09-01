module Test.Examples.Event.OnceAndAltCombination where

import Debug
import FRP.Event
import FRP.Event.Class
import Prelude
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
import Data.Number (e)
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
import Effect.Random (random)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (justNone, justOne, makeEvent, memoize, merge, subscribe)
import FRP.Event as Event
import FRP.Event.Class as EClass
import FRP.Event.Class as Event
import FRP.Event.Time (throttle, withTime)
import FRP.Poll as OptimizedPoll
import FRP.Poll.Unoptimized (APoll(..), Poll, poll, sample, step)
import FRP.Poll.Unoptimized as UnoptimizedPoll
import Foreign.Object.ST as STObject
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

-- push channel event1:          1  2                   3       4
-- push channel event2:                 +100    +200                     +300    +400
-- sampleOnLeft event1 event2:   _  _   _       _       203     204      _       _
-- sampleOnRight event1 event2:  _  _   102     202     _       _        304     404
-- apply event2 event1:          _  _   102     202     203     204      304     404

test_sampleOn :: Effect Unit
test_sampleOn = do
  supplier1 <- liftST $ createPure
  supplier2 <- liftST $ createPure

  -- Define the events
  let
    supplierCombined :: Event Int
    supplierCombined = EClass.once supplier1.event <|> supplier2.event

  -- Subscribe to the sampled events
  _ <- subscribe supplierCombined \v -> do
    log $ "supplierCombined GOT " <> show v

  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" supplier2.push 1
  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" supplier2.push 2
  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" supplier1.push 3
  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" supplier1.push 4
  -- vocalpush "2. PUSHING TO FIRST EVENT (SAMPLE) 2" eventAndPush_input1.push 2
  -- vocalpush "3. PUSHING TO SECOND EVENT (TRIGGER) +100" eventAndPush_input2.push (\i -> i + 100)
  -- vocalpush "4. PUSHING TO SECOND EVENT (TRIGGER) +200" eventAndPush_input2.push (\i -> i + 200)
  -- vocalpush "5. PUSHING TO FIRST EVENT (SAMPLE) 3" eventAndPush_input1.push 3
  -- vocalpush "6. PUSHING TO FIRST EVENT (SAMPLE) 4" eventAndPush_input1.push 4
  -- vocalpush "7. PUSHING TO SECOND EVENT (TRIGGER) +300" eventAndPush_input2.push (\i -> i + 300)
  -- vocalpush "8. PUSHING TO SECOND EVENT (TRIGGER) +400" eventAndPush_input2.push (\i -> i + 400)

  pure unit
  where
  vocalpush :: forall s. String -> (s -> ST Global Unit) -> s -> _ Unit
  vocalpush tag push value = do
    traceM (tag <> ": ")
    liftST $ push value
    traceM ("END")

main :: Effect Unit
main = test_sampleOn
