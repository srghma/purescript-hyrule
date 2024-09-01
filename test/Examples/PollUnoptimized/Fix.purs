module Test.Examples.PollUnoptimized.Fix where

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
import Effect.Exception (throw)
import Effect.Ref (modify)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (justNone, justOne, makeEvent, memoize, merge, subscribe)
import FRP.Event as Event
import FRP.Event.Time (throttle, withTime)
import FRP.Poll as OptimizedPoll
import FRP.Poll.Unoptimized (Poll, fixWithInitial, sample, sham, step)
import FRP.Poll.Unoptimized as UnoptimizedPoll
import Foreign.Object.ST as STObject
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

data Cow = C0 | C1 | C2 | C3 | C4

main :: Effect Unit
main = do
  surveymonger1 <- liftST $ createPure
  cowSupplier <- liftST $ createPure
  eventAndPush_input <- liftST $ createPure
  eventAndPush_sampler <- liftST $ createPure

  let
    (whoWantsToAnswer :: Poll Cow) = fix (\i -> i)
    -- (whoWantsToAnswer :: Poll Cow) = fix (\i -> i <|> eventAndPush_inputPoll)
    -- (whoWantsToAnswer :: Poll Cow) = fixWithInitial C1 (\i -> i <|> pure C2)
    -- (whoWantsToAnswer :: Poll Cow) = fixWithInitial C1 (\i -> i <|> step C2 cowSupplier.event)
    -- (whoWantsToAnswer :: Poll Cow) = fixWithInitial C1 (\i -> step C2 cowSupplier.event <|> i)
    -- (whoWantsToAnswer :: Poll Cow) = fixWithInitial C1 (\i -> i <|> sham cowSupplier.event)
    -- (whoWantsToAnswer :: Poll Cow) = fixWithInitial C1 (\i -> sham cowSupplier.event <|> i)
    -- (whoWantsToAnswer :: Poll Cow) = fix (\i -> i <|> step C1 cowSupplier.event)
    -- (whoWantsToAnswer :: Poll Cow) = fix (\i -> step C1 cowSupplier.event <|> i)
    -- (whoWantsToAnswer :: Poll Cow) = fix (\i -> i <|> sham cowSupplier.event)
    -- (whoWantsToAnswer :: Poll Cow) = fix (\i -> sham cowSupplier.event <|> i)

    (answers1 :: Event Int) =
      sample whoWantsToAnswer surveymonger1.event

  -- let (fixed_event :: Event Int) = Event.fix
  --       -- (\e -> trace e $ \_ -> map (\v -> v + 1) e)
  --       (\e -> filter (\a -> a == 2) e)

  liftST $ cowSupplier.push C0

  maxCalls <- Ref.new 100

  _ <- subscribe answers1 \v -> do
    log $ "answers1 GOT " <> show v
    maxCallsNow <- Ref.modify (\x -> x - 1) maxCalls
    if maxCallsNow < 0 then throw "STOP!!!" else pure unit

  liftST $ cowSupplier.push C3

  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" surveymonger1.push $ case _ of
    C0 -> 0
    C1 -> 1
    C2 -> 2
    C3 -> 3
    C4 -> 4
  liftST $ cowSupplier.push C4
  vocalpush "1. PUSHING TO FIRST EVENT (SAMPLE) 1" surveymonger1.push $ case _ of
    C0 -> 10
    C1 -> 11
    C2 -> 12
    C3 -> 13
    C4 -> 14

  -- vocalpush "PUSHING 1" eventAndPush_sampler.push ((+) 100)
  -- vocalpush "PUSHING 1" eventAndPush_input.push 1
  -- vocalpush "PUSHING 1" eventAndPush_input.push 2
  -- vocalpush "PUSHING 2" eventAndPush_sampler.push ((+)200)
  -- vocalpush "PUSHING 3" eventAndPush_sampler.push ((+)300)
  -- vocalpush "PUSHING 1" eventAndPush_input.push 3
  -- vocalpush "PUSHING 1" eventAndPush_input.push 4
  -- vocalpush "PUSHING 2" eventAndPush_sampler.push ((+)400)
  -- vocalpush "PUSHING 3" eventAndPush_sampler.push ((+)500)
  pure unit
  where
  vocalpush :: forall s. String -> (s -> ST Global Unit) -> s -> Effect Unit
  vocalpush tag push value = do
    traceM (tag <> ": " <> unsafeCoerce value)
    liftST $ push value
    traceM ("END")
