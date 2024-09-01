module Test.Examples.PollUnoptimized.Integral where

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
import FRP.Event.Time (throttle, withTime)
import FRP.Poll as OptimizedPoll
import FRP.Poll.Unoptimized (APoll(..), Poll, deflect, poll, rant, sample, sham, step, integral')
import FRP.Poll.Unoptimized as UnoptimizedPoll
import Foreign.Object.ST as STObject
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

test :: Effect Unit
test = do
  timeSupplier <- liftST $ createPure
  heightSupplier <- liftST $ createPure
  surveymonger <- liftST $ createPure

  let
    -- (area :: Poll Number) = integral' 0.0 (step 10.0 timeSupplier.event) (step 100.0 heightSupplier.event)
    (area :: Poll Number) = integral' 0.0 (sham timeSupplier.event) (sham heightSupplier.event)

    (answers :: Event String) = sample area surveymonger.event

  -- liftST $ cowSupplier.push C0

  _ <- subscribe answers \v -> log $ "answers1 GOT " <> show v

  -- liftST $ cowSupplier.push C1

  vocalpush "" surveymonger.push $ (\i->"1_"<>show i)
  liftST $ timeSupplier.push 1.0
  vocalpush "" surveymonger.push $ (\i->"1_"<>show i)
  liftST $ heightSupplier.push 1.0
  vocalpush "3" surveymonger.push $ (\i->"1_"<>show i)
  liftST $ timeSupplier.push 2.0
  vocalpush "" surveymonger.push $ (\i->"1_"<>show i)
  vocalpush "" surveymonger.push $ (\i->"1_"<>show i)
  liftST $ timeSupplier.push 3.0
  vocalpush "" surveymonger.push $ (\i->"1_"<>show i)
  -- liftST $ heightSupplier.push 2.0
  -- vocalpush "" surveymonger.push $ (\i->"1_"<>show i)
  -- liftST $ timeSupplier.push 15.0
  -- vocalpush "" surveymonger.push $ (\i->"1_"<>show i)

  -- vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" surveymonger1.push $ case _ of
  --   C0 -> 10
  --   C1 -> 11
  --   C2 -> 12
  --   C3 -> 13
  --   C4 -> 14
  -- -- liftST $ unsubscribe
  -- liftST $ cowSupplier.push C2
  -- -- liftST $ unsubscribe
  -- vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" surveymonger1.push $ case _ of
  --   C0 -> 20
  --   C1 -> 21
  --   C2 -> 22
  --   C3 -> 23
  --   C4 -> 24
  -- -- liftST $ unsubscribe
  -- liftST $ cowSupplier.push C3

  -- vocalpush "2. PUSHING TO FIRST EVENT (SAMPLE) 2" eventAndPush_input1.push 2
  -- vocalpush "3. PUSHING TO SECOND EVENT (TRIGGER) +100" eventAndPush_input2.push (\i -> i + 100)
  -- vocalpush "4. PUSHING TO SECOND EVENT (TRIGGER) +200" eventAndPush_input2.push (\i -> i + 200)
  -- vocalpush "5. PUSHING TO FIRST EVENT (SAMPLE) 3" eventAndPush_input1.push 3
  -- vocalpush "6. PUSHING TO FIRST EVENT (SAMPLE) 4" eventAndPush_input1.push 4
  -- vocalpush "7. PUSHING TO SECOND EVENT (TRIGGER) +300" eventAndPush_input2.push (\i -> i + 300)
  -- vocalpush "8. PUSHING TO SECOND EVENT (TRIGGER) +400" eventAndPush_input2.push (\i -> i + 400)

  pure unit
  where
  vocalpush :: forall s. String -> (s -> ST Global Unit) -> s -> Effect Unit
  vocalpush tag push value = do
    traceM (tag <> ": ")
    liftST $ push value
    traceM ("END")

main :: Effect Unit
main = test
