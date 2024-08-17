module FRP.Event
  ( Event
  , EventIO
  , EventIOO
  , PureEventIO
  , PureEventIOO
  , Subscriber(..)
  , EventfulProgram
  , ProgramfulEvent
  , fastForeachE
  , fastForeachST
  , fastForeachThunkST
  , fastForeachThunkE
  , justOne
  , justOneM
  , justMany
  , justManyM
  , justNone
  , create
  , createPure
  , createTagged
  , createPureTagged
  , mailbox
  , mailbox'
  , mailboxPure
  , mailboxPure'
  , mailboxS
  , mailboxS'
  , mailboxPureS
  , mailboxPureS'
  , makeEvent
  , makeEventE
  , memoize
  , merge
  , mergeMap
  , module Class
  , subscribe
  , subscribeO
  , foldObj
  , foldArr
  ) where

import Prelude

import Control.Alternative (class Alt, class Plus)
import Control.Apply (lift2)
import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (Region, ST)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as STRef
import Control.Monad.ST.Uncurried (STFn1, STFn2, STFn3, mkSTFn1, runSTFn1, runSTFn2, runSTFn3)
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
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import FRP.Event.Class (class Filterable, class IsEvent, count, filterMap, fix, fold, folded, gate, gateBy, keepLatest, mapAccum, sampleOnRight, sampleOnRight_, withLast) as Class
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject
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
newtype Event a = Event (STFn1 (EffectFn1 a Unit) Global (ST Global Unit))

instance functorEvent :: Functor Event where
  map f (Event e) = Event (mkSTFn1 (\effectfulCallback -> runSTFn1 e (mkEffectFn1 (\a -> runEffectFn1 effectfulCallback (f a)))))

instance functorWithIndexEvent :: FunctorWithIndex Int Event where
  mapWithIndex f e = Class.mapAccum (\a b -> Tuple (a + 1) (f a b)) 0 e

type EventfulProgram a = Free (Compose (ST Global) (Tuple (Array a))) Unit
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
    void $ liftST $ STArray.push u a
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
    void $ liftST $ STArray.push u a
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

once :: forall a. Event a -> Event a
once (Event e) =
  Event $ mkSTFn1 \effectfulCallback -> do
    latest <- STRef.new Nothing
    u <- STRef.new $ pure unit
    c <-
      runSTFn1 e $ mkEffectFn1 \a -> do
        o <- liftST $ STRef.read latest
        case o of
          Nothing -> do
            void $ liftST $ STRef.write (Just a) latest
            runEffectFn1 effectfulCallback a
            liftST $ join (STRef.read u)
          -- should not hit here
          Just _ -> pure unit
    void $ STRef.write c u
    o <- liftST $ STRef.read latest
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
          ( mkEffectFn1 \a -> case p a of
              Just y -> runEffectFn1 effectfulCallback y
              Nothing -> pure unit
          )
    )

sampleOnLeft :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOnLeft (Event e1) (Event e2) =
  Event $ mkSTFn1 \effectfulCallback -> do
    latest <- STRef.new Nothing
    c1 <-
      runSTFn1 e1 $ mkEffectFn1 \a -> do
        o <- liftST $ STRef.read latest
        for_ o (\f -> runEffectFn1 effectfulCallback (f a))
    c2 <-
      runSTFn1 e2 $ mkEffectFn1 \f -> do
        liftST $ void $ STRef.write (Just f) latest
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
      runSTFn1 e1 $ mkEffectFn1 \a -> do
        void $ liftST $ STRef.write (Just a) latest
    c2 <-
      runSTFn1 e2 $ mkEffectFn1 \f -> do
        o <- liftST $ STRef.read latest
        for_ o (\a -> runEffectFn1 effectfulCallback (f a))
    pure do
      c1
      c2

biSampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
biSampleOn (Event e1) (Event e2) =
  Event $ mkSTFn1 \effectfulCallback -> do
    latest1 <- STRef.new Nothing
    latest2 <- STRef.new Nothing
    c1 <-
      runSTFn1 e1 $ mkEffectFn1 \a -> do
        void $ liftST $ STRef.write (Just a) latest1
        res <- liftST $ STRef.read latest2
        for_ res (\f -> runEffectFn1 effectfulCallback (f a))
    c2 <-
      runSTFn1 e2 $ mkEffectFn1 \f -> do
        void $ liftST $ STRef.write (Just f) latest2
        res <- liftST $ STRef.read latest1
        for_ res (\a -> runEffectFn1 effectfulCallback (f a))
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
      runSTFn1 e $ mkEffectFn1 \(Event inner) -> liftST do
        ci <- STRef.read cancelInner
        ci
        c <- runSTFn1 inner effectfulCallback
        void $ liftST $ STRef.write c cancelInner
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
    c1 <- runSTFn1 e0 (mkEffectFn1 push)
    pure do
      c1
      c2

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe
  :: forall a
   . Event a
  -> (a -> Effect Unit)
  -> Effect (Effect Unit)
subscribe (Event e) effectfulCallback = liftST $ map liftST $ runSTFn1 e (mkEffectFn1 effectfulCallback)

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribeO
  :: forall a
   . EffectFn2 (Event a) (EffectFn1 a Unit) (Effect Unit)
subscribeO = mkEffectFn2 \(Event e) effectfulCallback -> liftST $ map liftST $ runSTFn1 e effectfulCallback

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
makeEvent
  :: forall a
   . ((forall b. Event b -> (b -> EventfulProgram a) -> ST Global (ST Global Unit)) -> ST Global (ST Global Unit))
  -> Event a
makeEvent i = Event $ mkSTFn1 \effectfulCallback ->
  i \(Event e) kx -> do
    c <- runSTFn1 e $ mkEffectFn1 \ii -> do
      let
        go = resume >>> case _ of
          Right _ -> pure $ Done unit
          Left (Compose prog) -> do
            Tuple a rest <- liftST prog
            runEffectFn2 fastForeachE a effectfulCallback
            pure $ Loop rest
      tailRecM go (kx ii)
    pure c

newtype Subscriber = Subscriber (forall b. STFn2 (Event b) (STFn1 b Global Unit) Global (ST Global Unit))

type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create :: forall a. ST Global (EventIO a)
create = create_ ""

createTagged :: forall a. String -> ST Global (EventIO a)
createTagged = create_

createPure :: forall a. ST Global (PureEventIO a)
createPure = unsafeCoerce $ create_ ""

createPureTagged :: forall a. String -> ST Global (PureEventIO a)
createPureTagged = unsafeCoerce create_

type EventIOO i o =
  { event :: Event o
  , push :: EffectFn1 i Unit
  }

foreign import data ObjHack :: Region -> Type -> Type

type role ObjHack nominal representational

foreign import objHack :: forall a r. String -> ST r (ObjHack r a)
foreign import insertObjHack :: forall a r. STFn3 Int a (ObjHack r a) Global Unit
foreign import deleteObjHack :: forall a r. STFn2 Int (ObjHack r a) Global Unit

memoize :: forall a. Event a -> Effect { event :: Event a, unsubscribe :: Effect Unit }
memoize e = do
  { event, push } <- liftST create
  unsubscribe <- subscribe e push
  pure { event, unsubscribe }

create_
  :: forall a
   . String
  -> ST Global (EventIO a)
create_ tag = do
  subscribers <- objHack tag
  idx <- STRef.new 0
  pure
    { event:
        Event $ mkSTFn1 \effectfulCallback -> do
          recordOfEffectfulCallbacks <- STRef.new effectfulCallback
          ix <- STRef.read idx
          runSTFn3 insertObjHack ix recordOfEffectfulCallbacks subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            void $ STRef.write mempty recordOfEffectfulCallbacks
            runSTFn2 deleteObjHack ix subscribers
            pure unit
    , push:
        \a -> do
          runEffectFn2 fastForeachOhE subscribers $ mkEffectFn1 \recordOfEffectfulCallbacks -> do
            effectfulCallback <- liftST $ STRef.read recordOfEffectfulCallbacks
            runEffectFn1 effectfulCallback a
    }

type PureEventIO a =
  { event :: Event a
  , push :: a -> ST Global Unit
  }

type PureEventIOO r a =
  { event :: Event a
  , push :: STFn1 a r Unit
  }

mailbox :: forall a b. Ord a => ST Global { push :: { address :: a, payload :: b } -> Effect Unit, event :: a -> Event b }
mailbox = do
  { push, event } <- mailbox'
  pure
    { push: \ap -> runEffectFn1 push ap
    , event
    }

mailboxPure :: forall a b. Ord a => ST Global { push :: { address :: a, payload :: b } -> ST Global Unit, event :: a -> Event b }
mailboxPure = (unsafeCoerce :: (forall a b. Ord a => ST Global { push :: { address :: a, payload :: b } -> Effect Unit, event :: a -> Event b }) -> (Ord a => ST Global { push :: { address :: a, payload :: b } -> ST Global Unit, event :: a -> Event b })) mailbox

mailboxPure' :: forall a b. Ord a => ST Global { push :: STFn1 { address :: a, payload :: b } Global Unit, event :: a -> Event b }
mailboxPure' = (unsafeCoerce :: (Ord a => ST Global { push :: EffectFn1 { address :: a, payload :: b } Unit, event :: a -> Event b }) -> (Ord a => ST Global { push :: STFn1 { address :: a, payload :: b } Global Unit, event :: a -> Event b })) mailbox'

mailbox' :: forall a b. Ord a => ST Global { push :: EffectFn1 { address :: a, payload :: b } Unit, event :: a -> Event b }
mailbox' = do
  r <- STRef.new Map.empty
  idx <- STRef.new 0
  pure
    { event: \a ->
        Event $ mkSTFn1 \effectfulCallback -> do
          o <- liftST $ STRef.read r
          subscribers <- case Map.lookup a o of
            Nothing -> do
              oh <- objHack ""
              void $ STRef.modify (Map.insert a oh) r
              pure oh
            Just s -> pure s
          recordOfEffectfulCallbacks <- STRef.new effectfulCallback
          ix <- STRef.read idx
          runSTFn3 insertObjHack ix recordOfEffectfulCallbacks subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            void $ STRef.write mempty recordOfEffectfulCallbacks
            runSTFn2 deleteObjHack ix subscribers
            pure unit
    , push: mkEffectFn1 \{ address, payload } -> do
        o <- liftST $ STRef.read r
        case Map.lookup address o of
          Nothing -> pure unit
          Just subscribers -> runEffectFn2 fastForeachOhE subscribers $ mkEffectFn1 \recordOfEffectfulCallbacks -> do
            effectfulCallback <- liftST $ STRef.read recordOfEffectfulCallbacks
            runEffectFn1 effectfulCallback payload
        pure unit
    }

mailboxS :: forall b. ST Global { push :: { address :: String, payload :: b } -> Effect Unit, event :: String -> Event b }
mailboxS = do
  { push, event } <- mailboxS'
  pure
    { push: \ap -> runEffectFn1 push ap
    , event
    }

mailboxPureS :: forall b. ST Global { push :: { address :: String, payload :: b } -> ST Global Unit, event :: String -> Event b }
mailboxPureS = (unsafeCoerce :: (forall b. ST Global { push :: { address :: String, payload :: b } -> Effect Unit, event :: String -> Event b }) -> (ST Global { push :: { address :: String, payload :: b } -> ST Global Unit, event :: String -> Event b })) mailbox

mailboxPureS' :: forall b. ST Global { push :: STFn1 { address :: String, payload :: b } Global Unit, event :: String -> Event b }
mailboxPureS' = (unsafeCoerce :: (ST Global { push :: EffectFn1 { address :: String, payload :: b } Unit, event :: String -> Event b }) -> (ST Global { push :: STFn1 { address :: String, payload :: b } Global Unit, event :: String -> Event b })) mailbox'

mailboxS' :: forall b. ST Global { push :: EffectFn1 { address :: String, payload :: b } Unit, event :: String -> Event b }
mailboxS' = do
  r <- STObject.new
  idx <- STRef.new 0
  pure
    { event: \a ->
        Event $ mkSTFn1 \effectfulCallback -> do
          o <- liftST $ STObject.peek a r
          subscribers <- case o of
            Nothing -> do
              oh <- objHack ""
              void $ STObject.poke a oh r
              pure oh
            Just s -> pure s
          recordOfEffectfulCallbacks <- STRef.new effectfulCallback
          ix <- STRef.read idx
          runSTFn3 insertObjHack ix recordOfEffectfulCallbacks subscribers
          void $ STRef.modify (_ + 1) idx
          pure do
            void $ STRef.write mempty recordOfEffectfulCallbacks
            runSTFn2 deleteObjHack ix subscribers
            pure unit
    , push: mkEffectFn1 \{ address, payload } -> do
        o <- liftST $ STObject.peek address r
        case o of
          Nothing -> pure unit
          Just subscribers -> runEffectFn2 fastForeachOhE subscribers $ mkEffectFn1 \recordOfEffectfulCallbacks -> do
            effectfulCallback <- liftST $ STRef.read recordOfEffectfulCallbacks
            runEffectFn1 effectfulCallback payload
        pure unit
    }

--
foreign import fastForeachThunkST :: STFn1 (Array (ST Global Unit)) Global Unit

fastForeachThunkE :: EffectFn1 (Array (Effect Unit)) Unit
fastForeachThunkE = unsafeCoerce fastForeachThunkST

foreign import fastForeachE :: forall a. EffectFn2 (Array a) (EffectFn1 a Unit) Unit

fastForeachST :: forall a. STFn2 (Array a) (STFn1 a Global Unit) Global Unit
fastForeachST = unsafeCoerce fastForeachE

foreign import fastForeachOhE :: forall a r. EffectFn2 (ObjHack r a) (EffectFn1 a Unit) Unit

makeEventE :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Effect { event :: Event a, unsubscribe :: Effect Unit }
makeEventE e = do
  { event, push } <- liftST create
  unsubscribe <- e push
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
