module Test.Examples.PollUnoptimized.Sham where

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
import FRP.Poll.Unoptimized (APoll(..), Poll, poll, sample, sham, step)
import FRP.Poll.Unoptimized as UnoptimizedPoll
import Foreign.Object.ST as STObject
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

-- A surveymonger named event arrives with a survey called a -> b. Some entities a need to take the survey (could be a survey for cows, chairs, humans, whatever) and produce responses b. event tasks you with administering the survey. As your job is just that, you don't care what the results are, in fact, you have no idea what they mean. But you do know how to run a survey. So you consult the cows, or chairs, or humans, aka a, gather a bunch of responses, aka b, and report them back, aka event b.

data Cow = C1 | C2

howMuchEnjoyDeath :: Cow -> Int
howMuchEnjoyDeath C1 = 1
howMuchEnjoyDeath C2 = 2

test_sampleOn :: Effect Unit
test_sampleOn = do
  cowSupplier <- liftST $ createPure
  surveymonger1 <- liftST $ createPure
  surveymonger2 <- liftST $ createPure

  -- Define the events
  let
    whoWantsToAnswer :: Poll Cow
    -- whoWantsToAnswer = pure C1 <|> pure C2
    -- whoWantsToAnswer = step C1 cowSupplier.event
    whoWantsToAnswer = sham cowSupplier.event

    (answers1 :: Event Int) =
      sample whoWantsToAnswer surveymonger1.event

    (answers2 :: Event Int) =
      sample whoWantsToAnswer surveymonger2.event

  -- Subscribe to the sampled events
  _ <- subscribe answers1 \v -> do
    log $ "answers1 GOT " <> show v
  _ <- subscribe answers2 \v -> do
    log $ "answers2 GOT " <> show v

  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" surveymonger1.push $ case _ of
    C1 -> 1
    C2 -> 2
  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" surveymonger1.push $ case _ of
    C1 -> 11
    C2 -> 12
  liftST $ cowSupplier.push C2
  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" surveymonger1.push $ case _ of
    C1 -> 21
    C2 -> 22

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
