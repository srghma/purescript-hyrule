module Test.Examples.Event.Fold where

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

-- input  :  1   2   3   4
-- folded :  1   3   6   10

main :: Effect Unit
main = test_fold

test_fold :: Effect Unit
test_fold = do
  eventAndPush_input <- liftST $ createPure

  -- Define an initial value and a folding function
  let initialValue = 0
  let foldingFunction = \acc v -> acc + v

  -- Create the folded event
  let (folded_event :: Event Int) = fold foldingFunction initialValue eventAndPush_input.event

  -- Subscribe to the folded event to log results
  folded_event_unsubscribe <- subscribe folded_event \v -> do
    log $ "folded_event GOT " <> show v

  -- Push values to the input event
  vocalpush "PUSHING 1" eventAndPush_input.push 1
  vocalpush "PUSHING 2" eventAndPush_input.push 2
  vocalpush "PUSHING 3" eventAndPush_input.push 3
  vocalpush "PUSHING 4" eventAndPush_input.push 4

  -- Output status
  traceM "folded_event_unsubscribe" <> folded_event_unsubscribe
  pure unit
  where
  vocalpush :: forall s. Show s => String -> (s -> ST Global Unit) -> s -> _ Unit
  vocalpush tag push value = do
    traceM (tag <> ": " <> show value)
    liftST $ push value
    traceM ("END")
