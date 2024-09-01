-- @inline export wrapEvent always

module FRP.EventPure where

-- ( Event
-- , PureEventIO
-- , PureEventIOO
-- , EventfulProgram
-- , ProgramfulEvent
-- , PureEventIO
-- , PureEventIOO
-- , Subscriber(..)
-- , TupleArrayInSt
-- , create
-- , createPure
-- , createPureTagged
-- , createTagged
-- , fastForeachE
-- , fastForeachST
-- , fastForeachThunkE
-- , fastForeachThunkST
-- , foldArr
-- , foldObj
-- , justMany
-- , justManyM
-- , justNone
-- , justOne
-- , justOneM
-- , mailbox
-- , mailbox'
-- , mailboxPure
-- , mailboxPure'
-- , mailboxPureS
-- , mailboxPureS'
-- , mailboxS
-- , mailboxS'
-- , makeEvent
-- , makeEventE
-- , memoize
-- , merge
-- , mergeMap
-- , module Class
-- , subscribe
-- , subscribeO
-- , subscribePure
-- -- , subscribePureSt
-- -- , unwrapEvent
-- , wrapEvent
-- ) where

import Prelude

import Control.Alternative (class Alt, class Plus)
import Control.Apply (lift2)
import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (Region, ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as STRef
import Control.Monad.ST.Uncurried (STFn1, STFn2, STFn3, mkSTFn1, mkSTFn2, runSTFn1, runSTFn2, runSTFn3)
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Compactable (class Compactable)
import Data.Either (Either(..), either, hush)
import Data.Filterable as Filterable
import Data.Foldable (for_)
import Data.Functor.Compose (Compose(..))
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import FRP.Event (ObjHack, deleteObjHack, fastForeachE, fastForeachOhE, fastForeachOhST, fastForeachST, fastForeachThunkST, insertObjHack, objHack)
import FRP.Event.Class (class Filterable, class IsEvent, count, filterMap, fix, fold, folded, gate, gateBy, keepLatest, mapAccum, sampleOnRight, sampleOnRight_, withLast) as Class
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject
import Safe.Coerce (class Coercible, coerce)
import Unsafe.Coerce (unsafeCoerce)

-- | An `Event` represents a collection of discrete occurrences with associated
-- | times. Conceptually, an `Event` is a (possibly-infinite) list of values-and-times:
-- |
-- | ```purescript
-- | type Event a = List { value :: a, time :: Time }
-- | ```
-- |
-- | Events are created from real events like timers or mouse clicks, and then
-- | combined using the various functions and instances provided in this module.
-- |
-- | Events are consumed by providing a callback using the `subscribe` function.
-- |
-- | Without optimizations it reads as
-- |
-- | ```purescript
-- | type Event a = (a -> ST Global Unit) -> ST Global (ST Global Unit))
-- | ```

-- newtype AEvent callbackFn a = Event (STFn1 callbackFn Global (ST Global Unit))

-- type Event a = AEvent (STFn1 a Global Unit) a
-- type EventPure a = AEvent (STFn1 a Global Unit) a

-- -- TODO for createPure
-- instance functorEvent :: CallbackFn callbackFn => Functor (AEvent callbackFn) where
--   map f (Event e) = Event (mkSTFn1 (\effectfulCallback -> runSTFn1 e (mkSTFn1 (\a -> runSTFn1 effectfulCallback (f a)))))

newtype Event a = Event (STFn1 (STFn1 a Global Unit) Global (ST Global Unit))

-- TODO for createPure
instance functorEvent :: Functor Event where
  map f (Event e) = Event (mkSTFn1 (\effectfulCallback -> runSTFn1 e (mkSTFn1 (\a -> runSTFn1 effectfulCallback (f a)))))

instance functorWithIndexEvent :: FunctorWithIndex Int Event where
  mapWithIndex f e = Class.mapAccum (\a b -> Tuple (a + 1) (f a b)) 0 e

type TupleArrayInSt a = Compose (ST Global) (Tuple (Array a))
type EventfulProgram a = Free (TupleArrayInSt a) Unit

-- What this is saying is that, if you want to make an event:

-- - You have to return something to turn the event on and off: ST Global (ST Global Unit).
-- - All you have to work with internally is a function that takes an event and allows you to execute a program on each emission. Every step of this program can elect to emit Array a's worth of events at each step of the program.

-- type EventfulProgram a = Free (Compose (ST Global) (Tuple (Array a))) Unit
type ProgramfulEvent b = forall a. Free (Compose (ST Global) (Tuple (Array a))) b

instance compactableEvent :: Compactable Event where
  compact = filter identity
  separate xs =
    { left:
        filter
          ( case _ of
              Left x -> Just x
              Right _ -> Nothing
          )
          xs
    , right:
        filter
          ( case _ of
              Right x -> Just x
              Left _ -> Nothing
          )
          xs
    }

filter' :: forall a. (a → Boolean) → Event a → Event a
filter' f =
  filter
    ( \a -> case f a of
        true -> Just a
        false -> Nothing
    )

instance filterableEvent :: Filterable.Filterable Event where
  filter = filter'
  filterMap = filter
  partition p xs = { yes: filter' p xs, no: filter' (not <<< p) xs }
  partitionMap f xs =
    { left: Filterable.filterMap (either Just (const Nothing) <<< f) xs
    , right: Filterable.filterMap (hush <<< f) xs
    }

instance altEvent :: Alt Event where
  alt (Event f) (Event g) =
    Event $ mkSTFn1 \effectfulCallback -> ado
      c1 <- runSTFn1 f effectfulCallback
      c2 <- runSTFn1 g effectfulCallback
      in
        do
          c1
          c2

-- | Merge together several events. This has the same functionality
-- | as `oneOf`, but it is faster and less prone to stack explosions.
merge :: forall a. Array (Event a) → Event a
merge f = Event $ mkSTFn1 \effectfulCallback -> do
  a <- STArray.new
  runSTFn2 fastForeachST f $ mkSTFn1 \(Event i) -> do
    u <- runSTFn1 i effectfulCallback
    void $ STArray.push u a
  pure do
    o <- liftST (STArray.freeze a)
    runSTFn1 fastForeachThunkST o

-- | Merge together several events and map on the event. This has the same functionality
-- | as `oneOf`, but it is faster and less prone to stack explosions.
mergeMap :: forall a b. (a -> Event b) -> Array a → Event b
mergeMap f0 f = Event $ mkSTFn1 \effectfulCallback -> do
  a <- STArray.new
  runSTFn2 fastForeachST f $ mkSTFn1 \x -> do
    let (Event i) = f0 x
    u <- runSTFn1 i effectfulCallback
    void $ STArray.push u a
  pure do
    o <- liftST (STArray.freeze a)
    runSTFn1 fastForeachThunkST o

instance plusEvent :: Plus Event where
  empty = Event $ mkSTFn1 \_ -> pure (pure unit)

instance applyEvent :: Apply Event where
  apply a b = biSampleOn a ((#) <$> b)

instance eventIsEvent :: Class.IsEvent Event where
  keepLatest = keepLatest
  sampleOnRight = sampleOnRight
  sampleOnLeft = sampleOnLeft
  fix = fix
  once = once

instance semigroupEvent :: (Semigroup a) => Semigroup (Event a) where
  append = lift2 append

-- | Emits an event only once

once :: forall a. Event a -> Event a
once (Event e) =
  Event $ mkSTFn1 \effectfulCallback -> do
    latest <- STRef.new Nothing
    u <- STRef.new $ pure unit
    c <-
      runSTFn1 e $ mkSTFn1 \a -> do
        o <- STRef.read latest
        case o of
          Nothing -> do
            void $ STRef.write (Just a) latest
            runSTFn1 effectfulCallback a
            join (STRef.read u)
          -- should not hit here
          Just _ -> pure unit
    void $ STRef.write c u
    o <- STRef.read latest
    case o of
      Just _ -> c
      _ -> pure unit
    pure do
      c

-- | Create an `Event` which only fires when a predicate holds.
filter :: forall a b. (a -> Maybe b) -> Event a -> Event b
filter p (Event e) =
  Event
    ( mkSTFn1 \effectfulCallback ->
        runSTFn1 e
          ( mkSTFn1 \a -> case p a of
              Just y -> runSTFn1 effectfulCallback y
              Nothing -> pure unit
          )
    )

sampleOnLeft :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnLeft (Event e1) (Event e2) =
  Event $ mkSTFn1 \effectfulCallback -> do
    latest <- STRef.new Nothing
    c1 <-
      runSTFn1 e1 $ mkSTFn1 \a -> do
        o <- STRef.read latest
        for_ o (\f -> runSTFn1 effectfulCallback (f a))
    c2 <-
      runSTFn1 e2 $ mkSTFn1 \f -> do
        void $ STRef.write (Just f) latest
    pure do
      c1
      c2

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOnRight :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnRight (Event e1) (Event e2) =
  Event $ mkSTFn1 \effectfulCallback -> do
    latest <- STRef.new Nothing
    c1 <-
      runSTFn1 e1 $ mkSTFn1 \a -> do
        void $ STRef.write (Just a) latest
    c2 <-
      runSTFn1 e2 $ mkSTFn1 \f -> do
        o <- STRef.read latest
        for_ o (\a -> runSTFn1 effectfulCallback (f a))
    pure do
      c1
      c2

biSampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
biSampleOn (Event e1) (Event e2) =
  Event $ mkSTFn1 \effectfulCallback -> do
    latest1 <- STRef.new Nothing
    latest2 <- STRef.new Nothing
    c1 <-
      runSTFn1 e1 $ mkSTFn1 \a -> do
        void $ STRef.write (Just a) latest1
        res <- STRef.read latest2
        for_ res (\f -> runSTFn1 effectfulCallback (f a))
    c2 <-
      runSTFn1 e2 $ mkSTFn1 \f -> do
        void $ STRef.write (Just f) latest2
        res <- STRef.read latest1
        for_ res (\a -> runSTFn1 effectfulCallback (f a))
    pure do
      c1
      c2

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
keepLatest :: forall a. Event (Event a) -> Event a
keepLatest (Event e) =
  Event $ mkSTFn1 \effectfulCallback -> do
    cancelInner <- STRef.new (pure unit)
    cancelOuter <-
      runSTFn1 e $ mkSTFn1 \(Event inner) -> liftST do
        ci <- STRef.read cancelInner
        ci
        c <- runSTFn1 inner effectfulCallback
        void $ STRef.write c cancelInner
    pure do
      ci <- STRef.read cancelInner
      ci
      cancelOuter

-- | Compute a fixed point
fix :: forall i. (Event i -> Event i) -> Event i
fix f =
  Event $ mkSTFn1 \effectfulCallback -> do
    { event, push } <- create
    let Event e0 = f event
    let Event e1 = event
    c2 <- runSTFn1 e1 effectfulCallback
    c1 <- runSTFn1 e0 (mkSTFn1 push)
    pure do
      c1
      c2

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe
  :: forall a
   . Event a
  -> (a -> ST Global Unit)
  -> ST Global (ST Global Unit)
subscribe (Event e) effectfulCallback = runSTFn1 e (mkSTFn1 effectfulCallback)

-- | Subscribe to an `Event` by providing a callback. (optimized)
-- |
-- | `subscribe` returns a canceller function.
-- subscribeO
--   :: forall a
--    . STFn2 (Event a) (STFn1 a Global Unit) (ST Global Unit)
-- subscribeO = mkSTFn2 \(Event e) effectfulCallback -> map runSTFn1 e effectfulCallback

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
-- subscribePureSt -- UNSAFE!!!!
--   :: forall a
--    . Event a
--   -> (a -> ST Global Unit)
--   -> ST Global (ST Global Unit)
-- subscribePureSt (Event e) effectfulCallback = runSTFn1 e (mkSTFn1 effectfulCallback)

-- unwrapEvent = subscribePureSt

-- wrapEvent -- THIS FUNCTION IS IMPOSSIBLE TO USE!!! YOU CANNOT RUN `a -> ST Global Unit` IN ST
--   :: forall a
--    . ((a -> ST Global Unit) -> ST Global (ST Global Unit))
--   -> Event a
-- -- wrapEvent f = f \effectfulCallback -> Event $ mkSTFn1 ( ?a)
-- wrapEvent f = Event $ mkSTFn1 (f <<< runSTFn1)

wrapEvent
  :: forall a
   . ((a -> ST Global Unit) -> ST Global (ST Global Unit)) -- AHHAHAH UNSAFE
  -> Event a
wrapEvent f = Event $ mkSTFn1 (f <<< runSTFn1)

-- instance Coercible (Event a) ((a -> ST Global Unit) -> ST Global (ST Global Unit)) where
--   wrap = wrapEvent
--   unwrap = unwrapEvent

-- instance Newtype (Event a) ((a -> ST Global Unit) -> ST Global (ST Global Unit)) where
--   wrap = wrapEvent
--   unwrap = unwrapEvent

-- | Subscribe to an `Event` by providing a callback. (optimized)
-- |
-- | `subscribe` returns a canceller function.
subscribePureStO
  :: forall a
   . Event a
  -> STFn1 (STFn1 a Global Unit) Global (ST Global Unit)
subscribePureStO (Event e) = e

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribePure -- could be called just unwrapEvent
  :: forall a
   . Event a
  -> (a -> ST Global Unit) -- HAH should not be possible
  -> ST Global (ST Global Unit)
subscribePure (Event e) callback = runSTFn1 e (mkSTFn1 $ \a -> callback a)

justOne :: forall a. a -> EventfulProgram a
justOne a = liftF (Compose (pure (Tuple [ a ] unit)))

justOneM :: forall a. ST Global a -> EventfulProgram a
justOneM a = liftF (Compose (a <#> \a' -> Tuple [ a' ] unit))

justMany :: Array ~> EventfulProgram
justMany a = liftF (Compose (pure (Tuple a unit)))

justManyM :: forall a. ST Global (Array a) -> EventfulProgram a
justManyM a = liftF (Compose (a <#> \a' -> Tuple a' unit))

justNone :: ST Global ~> ProgramfulEvent
justNone st = liftF (Compose (st <#> \st' -> (Tuple [] st')))

-- | Make an `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.

-- | Event a - covariant position (matter)
-- | ((forall b. Event b -> (b -> EventfulProgram a) -> ST Global (ST Global Unit)) -> ST Global (ST Global Unit)) - contravariant position (antimatter)
-- | (forall b. Event b -> (b -> EventfulProgram a) -> ST Global (ST Global Unit)) - covariant position (matter, `makeEvent` will give it to You)
-- | Event b - contravariant position (antimatter, You should give it to `makeEvent`, You should have it)
makeEvent
  :: forall a
   . ((forall b. Event b -> (b -> EventfulProgram a) -> ST Global (ST Global Unit)) -> ST Global (ST Global Unit))
  -> Event a
makeEvent giveMe_eventb_btoEventfulProgram_iGive_StSt = Event $ mkSTFn1 \effectfulCallback ->
  giveMe_eventb_btoEventfulProgram_iGive_StSt \(Event b_createEvent) bToEventfulProgram -> do
    b_closeEvent <- runSTFn1 b_createEvent $ mkSTFn1 \b -> do
      let
        go
          :: Free (Compose (ST Global) (Tuple (Array a))) Unit
          -> ST Global (Step (Free (Compose (ST Global) (Tuple (Array a))) Unit) Unit)
        go = resume >>> case _ of
          Right _unit -> pure $ Done unit
          Left (Compose prog) -> do
            Tuple a rest <- liftST prog
            runSTFn2 fastForeachST a effectfulCallback
            pure $ Loop rest
      tailRecM go (bToEventfulProgram b)
    pure b_closeEvent

newtype Subscriber = Subscriber (forall b. STFn2 (Event b) (STFn1 b Global Unit) Global (ST Global Unit))

-- | Create an event and a function which supplies a value to that event.
create :: forall a. ST Global (PureEventIO a)
create = create_ ""

createTagged :: forall a. String -> ST Global (PureEventIO a)
createTagged = create_

createPure :: forall a. ST Global (PureEventIO a)
createPure = create

createPureTagged :: forall a. String -> ST Global (PureEventIO a)
createPureTagged = createTagged

-- | Will make the `ST Global (...)` in
-- | `type Event a = (a -> ST Global Unit) -> ST Global (ST Global Unit))`
-- | to be executed only once, no matter how many times `subscribe` was called
memoize :: forall a. Event a -> ST Global { event :: Event a, unsubscribe :: ST Global Unit }
memoize e = do
  { event, push } <- liftST create
  unsubscribe <- subscribe e push
  pure { event, unsubscribe }

create_
  :: forall a
   . String
  -> ST Global (PureEventIO a)
create_ tag = do
  subscribers <- objHack tag
  idx <- STRef.new 0
  pure
    { event:
        Event $ mkSTFn1 \effectfulCallback -> do
          recordOfSTfulCallbacks <- STRef.new effectfulCallback
          ix <- STRef.read idx
          runSTFn3 insertObjHack ix recordOfSTfulCallbacks subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            -- void $ STRef.write mempty recordOfSTfulCallbacks
            runSTFn2 deleteObjHack ix subscribers
            pure unit
    , push:
        \a -> do
          runSTFn2 fastForeachOhST subscribers $ mkSTFn1 \recordOfSTfulCallbacks -> do
            effectfulCallback <- STRef.read recordOfSTfulCallbacks
            runSTFn1 effectfulCallback a
    }

type PureEventIO a =
  { event :: Event a
  , push :: a -> ST Global Unit
  }

type PureEventIOO r a =
  { event :: Event a
  , push :: STFn1 a r Unit
  }

mailbox :: forall a b. Ord a => ST Global { push :: { address :: a, payload :: b } -> ST Global Unit, event :: a -> Event b }
mailbox = do
  { push, event } <- mailbox'
  pure
    { push: \ap -> runSTFn1 push ap
    , event
    }

mailboxPure :: forall a b. Ord a => ST Global { push :: { address :: a, payload :: b } -> ST Global Unit, event :: a -> Event b }
mailboxPure = (unsafeCoerce :: (forall a b. Ord a => ST Global { push :: { address :: a, payload :: b } -> ST Global Unit, event :: a -> Event b }) -> (Ord a => ST Global { push :: { address :: a, payload :: b } -> ST Global Unit, event :: a -> Event b })) mailbox

mailboxPure' :: forall a b. Ord a => ST Global { push :: STFn1 { address :: a, payload :: b } Global Unit, event :: a -> Event b }
mailboxPure' = (unsafeCoerce :: (Ord a => ST Global { push :: STFn1 { address :: a, payload :: b } Global Unit, event :: a -> Event b }) -> (Ord a => ST Global { push :: STFn1 { address :: a, payload :: b } Global Unit, event :: a -> Event b })) mailbox'

mailbox' :: forall a b. Ord a => ST Global { push :: STFn1 { address :: a, payload :: b } Global Unit, event :: a -> Event b }
mailbox' = do
  r <- STRef.new Map.empty
  idx <- STRef.new 0
  pure
    { event: \a ->
        Event $ mkSTFn1 \effectfulCallback -> do
          o <- STRef.read r
          subscribers <- case Map.lookup a o of
            Nothing -> do
              oh <- objHack ""
              void $ STRef.modify (Map.insert a oh) r
              pure oh
            Just s -> pure s
          recordOfSTfulCallbacks <- STRef.new effectfulCallback
          ix <- STRef.read idx
          runSTFn3 insertObjHack ix recordOfSTfulCallbacks subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            -- void $ STRef.write mempty recordOfSTfulCallbacks
            runSTFn2 deleteObjHack ix subscribers
            pure unit
    , push: mkSTFn1 \{ address, payload } -> do
        o <- STRef.read r
        case Map.lookup address o of
          Nothing -> pure unit
          Just subscribers -> runSTFn2 fastForeachOhST subscribers $ mkSTFn1 \recordOfSTfulCallbacks -> do
            effectfulCallback <- STRef.read recordOfSTfulCallbacks
            runSTFn1 effectfulCallback payload
        pure unit
    }

mailboxS :: forall b. ST Global { push :: { address :: String, payload :: b } -> ST Global Unit, event :: String -> Event b }
mailboxS = do
  { push, event } <- mailboxS'
  pure
    { push: \ap -> runSTFn1 push ap
    , event
    }

mailboxPureS :: forall b. ST Global { push :: { address :: String, payload :: b } -> ST Global Unit, event :: String -> Event b }
mailboxPureS = (unsafeCoerce :: (forall b. ST Global { push :: { address :: String, payload :: b } -> ST Global Unit, event :: String -> Event b }) -> (ST Global { push :: { address :: String, payload :: b } -> ST Global Unit, event :: String -> Event b })) mailbox

mailboxPureS' :: forall b. ST Global { push :: STFn1 { address :: String, payload :: b } Global Unit, event :: String -> Event b }
mailboxPureS' = (unsafeCoerce :: (ST Global { push :: STFn1 { address :: String, payload :: b } Global Unit, event :: String -> Event b }) -> (ST Global { push :: STFn1 { address :: String, payload :: b } Global Unit, event :: String -> Event b })) mailbox'

mailboxS' :: forall b. ST Global { push :: STFn1 { address :: String, payload :: b } Global Unit, event :: String -> Event b }
mailboxS' = do
  r <- STObject.new
  idx <- STRef.new 0
  pure
    { event: \a ->
        Event $ mkSTFn1 \effectfulCallback -> do
          o <- STObject.peek a r
          subscribers <- case o of
            Nothing -> do
              oh <- objHack ""
              void $ STObject.poke a oh r
              pure oh
            Just s -> pure s
          recordOfSTfulCallbacks <- STRef.new effectfulCallback
          ix <- STRef.read idx
          runSTFn3 insertObjHack ix recordOfSTfulCallbacks subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            -- void $ STRef.write mempty recordOfSTfulCallbacks
            runSTFn2 deleteObjHack ix subscribers
            pure unit
    , push: mkSTFn1 \{ address, payload } -> do
        o <- STObject.peek address r
        case o of
          Nothing -> pure unit
          Just subscribers -> runSTFn2 fastForeachOhST subscribers $ mkSTFn1 \recordOfSTfulCallbacks -> do
            effectfulCallback <- STRef.read recordOfSTfulCallbacks
            runSTFn1 effectfulCallback payload
        pure unit
    }

makeEventE :: forall a. ((a -> ST Global Unit) -> ST Global (ST Global Unit)) -> ST Global { event :: Event a, unsubscribe :: ST Global Unit }
makeEventE e = do
  { event, push } <- liftST create
  unsubscribe <- e push -- stop hot event
  pure { event, unsubscribe }

-- | A fast fold over an object
foldObj :: forall a b c. (forall r. STObject r b -> a -> ST r c) -> Event a -> Event c
foldObj f e = makeEvent \s -> do
  o <- STObject.new
  let
    go :: a -> EventfulProgram c
    go a = do
      justOneM (f o a)
  c <- s e go
  pure c

-- | A fast fold over an array
foldArr :: forall a b c. (forall r. STArray r b -> a -> ST r c) -> Event a -> Event c
foldArr f e = makeEvent \s -> do
  o <- STArray.new
  let
    go :: a -> EventfulProgram c
    go a = do
      justOneM (f o a)
  c <- s e go
  pure c
