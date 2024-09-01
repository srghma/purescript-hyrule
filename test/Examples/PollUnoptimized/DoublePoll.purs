module Test.Examples.PollUnoptimized.DoublePoll where

import Debug
import FRP.EventPure
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.Rec.Class (untilJust)
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
import Data.Newtype (class Newtype, over, under, unwrap)
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
import FRP.Event.Class as EClass
import FRP.Poll as OptimizedPoll
import FRP.Poll.Unoptimized (APoll(..), Poll, unAPoll)
import FRP.Poll.Unoptimized as UnoptimizedPoll
import FRP.EventPure (justNone, justOne, makeEvent, memoize, merge, subscribe)
import FRP.EventPure as Event
import Foreign.Object.ST as STObject
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.Event.EventTypes (offline)

-- emptyPoll ∷ ∀ a. Poll a
-- emptyPoll _ _ = pure (pure unit)
-- emptyPoll = APoll \eventAtoR -> wrapEvent \effectfulCallback -> pure (pure unit)
-- emptyPoll = APoll \eventAtoR -> subscribePure eventAtoR \effectfulCallback -> ?a

-- erraticPoll ∷ APoll MyEvent Int
-- erraticPoll = APoll $ \eventAtoR -> MyEvent \effectfulCallback -> do
--   _ ← eventAtoR \aToR -> do
--     r ← random
--     launchAff_ do -- IMPOSSIBLE, BC IT IS ST
--       delay (Milliseconds (r * 10_000.0))
--       liftEffect $ effectfulCallback (aToR 42)
--   pure (pure unit)

doublePoll :: APoll Event Int
doublePoll =
  let
    pollInsides :: forall r. Event (Int -> r) -> Event r
    pollInsides e =
      let
        eventInsides :: (r -> ST Global Unit) -> ST Global (ST Global Unit)
        eventInsides effectfulCallback = do
          _ <- subscribePure e \aToR -> effectfulCallback (aToR 42) *> effectfulCallback (aToR 43)
          pure (pure unit)
      in
        wrapEvent eventInsides -- UNSAFE
  in
    APoll pollInsides

doublizePollDoesWork :: forall a. Event a -> APoll Event a
doublizePollDoesWork = subscribe >>> \(eventA :: (a -> ST Global Unit) -> ST Global (ST Global Unit)) ->
  let
    pollInsides :: forall r. Event (a -> r) -> Event r
    pollInsides = subscribe >>> \(eventAtoR :: ((a -> r) -> ST Global Unit) -> ST Global (ST Global Unit)) ->
      wrapEvent \(reportR :: r -> ST Global Unit) -> eventA \(a :: a) -> do
        (unsubscribe :: ST Global Unit) <- eventAtoR \(aToR :: a -> r) -> reportR $ aToR a
        pure unit -- unsubscribe
  in
    APoll pollInsides

doublizePollDoesWork2 :: forall a. Event a -> APoll Event a
doublizePollDoesWork2 e =
  let
    pollInsides :: forall r. Event (a -> r) -> Event r
    pollInsides = subscribe >>> \(eventAtoR :: ((a -> r) -> ST Global Unit) -> ST Global (ST Global Unit)) ->
      wrapEvent \(reportR :: r -> ST Global Unit) -> do
        unsubscribe1 <- eventAtoR \(aToR :: a -> r) -> do
          unsubscribe <- subscribe e \(a :: a) -> reportR $ aToR a
          pure unit -- unsubscribe
        pure (unsubscribe1)
  in
    APoll pollInsides

doublizePollDoesWork3 :: forall a. Event a -> APoll Event a
doublizePollDoesWork3 eventA =
  let
    pollInsides :: forall r. Event (a -> r) -> Event r
    pollInsides eventAToR =
      wrapEvent \(reportR :: r -> ST Global Unit) -> do
        unsubscribe1 <- subscribe eventAToR \(aToR :: a -> r) -> do
          unsubscribe <- subscribe eventA \(a :: a) -> reportR $ aToR a
          pure unit -- unsubscribe
        pure unsubscribe1
  in
    APoll pollInsides

doublizePollDoesWork4 :: forall a. Event a -> APoll Event a
doublizePollDoesWork4 eventA =
  let
    pollInsides :: forall r. Event (a -> r) -> Event r
    pollInsides eventAToR =
      wrapEvent \(reportR :: r -> ST Global Unit) -> do
        unsubscribe1 <- subscribe eventA \(a :: a) -> do
          unsubscribe <- subscribe eventAToR \(aToR :: a -> r) -> reportR $ aToR a
          pure unit -- unsubscribe
        pure (unsubscribe1) -- (pure unit)
  in
    APoll pollInsides

doublizePoll2 :: forall a. Event a -> APoll Event a
doublizePoll2 eventA =
  let
    pollInsides :: forall r. Event (a -> r) -> Event r
    pollInsides eventAtoR =
      let
        eventInsides
          :: (Event a -> (a -> EventfulProgram r) -> ST Global (ST Global Unit))
          -> ST Global (ST Global Unit)
        eventInsides giveMe_eventb_btoEventfulProgram_iGive_StSt = do
          ref <- STRef.new Nothing
          unsubscribeEventAtoR <- subscribe eventAtoR \(aToR :: a -> r) -> void $ STRef.write (Just aToR) ref
          unsubscribeGive <- giveMe_eventb_btoEventfulProgram_iGive_StSt eventA \(a :: a) -> justManyM $ STRef.read ref >>=
            case _ of
              Just aToR ->
                let
                  r = aToR a
                in
                  pure [ r, r ]
              Nothing -> pure []
          pure do
            unsubscribeEventAtoR
            unsubscribeGive
      in
        makeEvent eventInsides
  in
    UnoptimizedPoll.poll pollInsides

-- eventA \(a :: a) -> reportR (aToR a) *> reportR (aToR a)
test_test :: Effect Unit
test_test = do
  -- Create input events
  (eventAndPush_keyMaker :: { event :: Event Int, push :: Int -> ST Global Unit }) <- liftST create

  let (pollUnwrapped :: Event (Int -> String) -> Event String) = unAPoll (doublizePoll2 eventAndPush_keyMaker.event)
  (eventAndPush_sampler :: { event :: Event (Int -> String), push :: (Int -> String) -> ST Global Unit }) <- liftST create

  let pollResultingEvent = pollUnwrapped eventAndPush_sampler.event

  pollResultingEvent_unsubscribe <- liftST $ subscribe pollResultingEvent \v -> do
    traceM $ "pollResultingEvent_sampler GOT " <> show v

  vocalpush "1" eventAndPush_sampler.push (\x -> "sampler1 func0 " <> show x)
  vocalpush "2" eventAndPush_keyMaker.push 0
  -- vocalpush "3" eventAndPush_sampler.push (\x -> "sampler1 func1 " <> show x)
  -- vocalpush "4" eventAndPush_sampler.push (\x -> "sampler1 func2 " <> show x)

  -- vocalpush "5" eventAndPush_keyMaker.push 1 -- will use last event handler
  -- vocalpush "6" eventAndPush_sampler.push (\x -> "sampler1 func3 " <> show x)
  pure unit
  where
  vocalpush :: forall s. String -> (s -> ST Global Unit) -> s -> Effect Unit
  vocalpush tag push value = do
    traceM (tag <> ": " <> unsafeCoerce value)
    liftST $ push value
    traceM "END"

main :: Effect Unit
main = test_test
