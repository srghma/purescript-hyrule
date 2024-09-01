module Test.Examples.Event.Fix where

import Debug
import FRP.Event
import FRP.Event.Class
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

main :: Effect Unit
main = do
  eventAndPush_input <- liftST $ createPure
  eventAndPush_sampler <- liftST $ createPure

  let
    (fixed_event :: Event Int) = Event.fix
      (\i -> map ((+) 1) i <|> eventAndPush_input.event) -- mergeWith (+) e

  -- let (fixed_event :: Event Int) = Event.fix
  --       -- (\e -> trace e $ \_ -> map (\v -> v + 1) e)
  --       (\e -> filter (\a -> a == 2) e)

  fixed_event_unsubscribe <- subscribe fixed_event \v -> do
    log $ "subscriber GOT " <> show v

  -- vocalpush "PUSHING 1" eventAndPush_sampler.push ((+) 100)
  vocalpush "PUSHING 1" eventAndPush_input.push 1
  -- vocalpush "PUSHING 1" eventAndPush_input.push 2
  -- vocalpush "PUSHING 2" eventAndPush_sampler.push ((+)200)
  -- vocalpush "PUSHING 3" eventAndPush_sampler.push ((+)300)
  -- vocalpush "PUSHING 1" eventAndPush_input.push 3
  -- vocalpush "PUSHING 1" eventAndPush_input.push 4
  -- vocalpush "PUSHING 2" eventAndPush_sampler.push ((+)400)
  -- vocalpush "PUSHING 3" eventAndPush_sampler.push ((+)500)

  traceM "fixed_event_unsubscribe" <> fixed_event_unsubscribe
  pure unit
  where
  vocalpush :: forall s. String -> (s -> ST Global Unit) -> s -> _ Unit
  vocalpush tag push value = do
    traceM (tag <> ": " <> unsafeCoerce value)
    liftST $ push value
    traceM ("END")
