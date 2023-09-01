module FRP.Poll
  ( APoll(..)
  , Poll
  , PollIO
  , PurePollIO
  , animate
  , create
  , createPure
  , derivative
  , derivative'
  , dredge
  , fixB
  , gate
  , gateBy
  , integral
  , integral'
  , mailbox
  , merge
  , mergeMap
  , poll
  , pollFromEvent
  , pollFromOptimizedRep
  , pollFromPoll
  , rant
  , deflect
  , sham
  , solve
  , solve'
  , solve2
  , solve2'
  , stRefToPoll
  , stToPoll
  , step
  , switcher
  , toPoll
  , unfold
  ) where

import Prelude

import Control.Alt (class Alt, alt, (<|>))
import Control.Apply (lift2)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal (ST)
import Control.Monad.ST.Internal as STRef
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Either (Either, either)
import Data.Filterable (eitherBool, maybeBool)
import Data.Filterable as Filterable
import Data.Foldable (foldr, oneOf, oneOfMap)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FRP.Event (class IsEvent, Event, fold, justMany, makeEvent, subscribe)
import FRP.Event as Event
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class as EClass
import FRP.Poll.Unoptimized as Poll
import Unsafe.Coerce (unsafeCoerce)

-- | `APoll` is the more general type of `Poll`, which is parameterized
-- | over some underlying `event` type.
-- |
-- | Normally, you should use `Poll` instead, but this type
-- | can also be used with other types of events, including the ones in the
-- | `Semantic` module.
data APoll event a
  = OnlyPure (Array a)
  | OnlyPoll (Poll.APoll event a)
  | OnlyEvent (event a)
  | PureAndPoll (Array a) (Poll.APoll event a)

-- | A `Poll` acts like a survey or poll. We get a series of questions in time
-- | of form `a -> b`, and we respond with `b`.
-- |
-- | We can construct a sample a `Poll` from some `Event`, combine `Poll`s
-- | using `Applicative`, and sample a final `Poll` on some other `Event`.
type Poll = APoll Event

instance functorAPoll :: Functor event => Functor (APoll event) where
  map f (PureAndPoll x y) = PureAndPoll (map f x) (map f y)
  map f (OnlyPure x) = OnlyPure (map f x)
  map f (OnlyEvent x) = OnlyEvent (map f x)
  map f (OnlyPoll y) = OnlyPoll (map f y)

instance functorWithIndexAPoll :: (IsEvent event, FunctorWithIndex Int event, Apply event, Poll.Pollable APoll event event, Poll.Pollable Poll.APoll event event) => FunctorWithIndex Int (APoll event) where
  mapWithIndex f (PureAndPoll x y) = PureAndPoll (mapWithIndex f x) (EClass.mapAccum (\a b -> Tuple (a + 1) (f a b)) (Array.length x) y)
  mapWithIndex f (OnlyPure x) = OnlyPure (mapWithIndex f x)
  mapWithIndex f (OnlyEvent x) = OnlyEvent (mapWithIndex f x)
  mapWithIndex f (OnlyPoll y) = OnlyPoll (EClass.mapAccum (\a b -> Tuple (a + 1) (f a b)) (0) y)

instance applyAPoll :: (Alt event, Apply event, IsEvent event, Poll.Pollable Poll.APoll event event) => Apply (APoll event) where
  apply a b = pollFromPoll (toPoll a <*> toPoll b)

instance applicativeAPoll :: (Apply event, Plus event, IsEvent event, Poll.Pollable Poll.APoll event event) => Applicative (APoll event) where
  pure a = OnlyPure [ a ]

instance semigroupAPoll :: (Apply event, Poll.Pollable Poll.APoll event event, Alt event, IsEvent event, Semigroup a) => Semigroup (APoll event a) where
  append = lift2 append

instance monoidAPoll :: (Apply event, Poll.Pollable Poll.APoll event event, Alt event, IsEvent event, Monoid a) => Monoid (APoll event a) where
  mempty = pure mempty

instance heytingAlgebraAPoll :: (Apply event, Poll.Pollable Poll.APoll event event, Alt event, IsEvent event, HeytingAlgebra a) => HeytingAlgebra (APoll event a) where
  tt = pure tt
  ff = pure ff
  not = map not
  implies = lift2 implies
  conj = lift2 conj
  disj = lift2 disj

instance semiringAPoll :: (Apply event, Alt event, Poll.Pollable Poll.APoll event event, IsEvent event, Semiring a) => Semiring (APoll event a) where
  zero = pure zero
  one = pure one
  add = lift2 add
  mul = lift2 mul

instance ringAPoll :: (Apply event, Poll.Pollable Poll.APoll event event, Alt event, IsEvent event, Ring a) => Ring (APoll event a) where
  sub = lift2 sub

pollFromPoll :: forall event. Poll.APoll event ~> APoll event
pollFromPoll i = OnlyPoll i

-- | Construct a `Poll` from its sampling function.
pollFromEvent :: forall event. EClass.IsEvent event => event ~> APoll event
pollFromEvent = OnlyEvent

pollFromOptimizedRep :: forall event a. EClass.IsEvent event => Array a -> event a -> APoll event a
pollFromOptimizedRep a i = PureAndPoll a (Poll.sham i)

poll :: forall event a. (forall b. event (a -> b) -> event b) -> APoll event a
poll f = OnlyPoll (Poll.poll f)

toPoll :: forall event. Alt event => Apply event => Plus event => IsEvent event => APoll event ~> Poll.APoll event
toPoll (PureAndPoll a b) = oneOfMap pure a <|> b
toPoll (OnlyEvent a) = Poll.sham a
toPoll (OnlyPure a) = oneOfMap pure a
toPoll (OnlyPoll b) = b

-- | Create a `Poll` which is updated when an `Event` fires, by providing
-- | an initial value.
step :: forall event a. IsEvent event => a -> event a -> APoll event a
step a e = PureAndPoll [ a ] $ Poll.poll \e0 -> EClass.sampleOnRight e e0

-- | Create a `Poll` which is updated when an `Event` fires, by providing
-- | an initial value and a function to combine the current value with a new event
-- | to create a new value.
unfold :: forall event a b. IsEvent event => (b -> a -> b) -> b -> event a -> APoll event b
unfold f a e = step a (fold f a e)

instance (Alt event, IsEvent event) => Alt (APoll event) where
  alt (OnlyPure a) (OnlyPure x) = OnlyPure (a <> x)
  alt (OnlyPure a) (OnlyEvent y) = PureAndPoll a (Poll.sham y)
  alt (OnlyPure a) (OnlyPoll y) = PureAndPoll a y
  alt (OnlyPure a) (PureAndPoll x y) = PureAndPoll (a <> x) y
  alt (OnlyEvent b) (OnlyPure x) = PureAndPoll x (Poll.sham b)
  alt (OnlyEvent b) (OnlyEvent y) = OnlyEvent (alt b y)
  alt (OnlyEvent b) (OnlyPoll y) = OnlyPoll (alt (Poll.sham b) y)
  alt (OnlyEvent b) (PureAndPoll x y) = PureAndPoll x (alt (Poll.sham b) y)
  alt (OnlyPoll b) (OnlyPure x) = PureAndPoll x b
  alt (OnlyPoll b) (OnlyEvent y) = OnlyPoll (alt b $ Poll.sham y)
  alt (OnlyPoll b) (OnlyPoll y) = OnlyPoll (alt b y)
  alt (OnlyPoll b) (PureAndPoll x y) = PureAndPoll x (alt b y)
  alt (PureAndPoll a b) (OnlyPure x) = PureAndPoll (a <> x) b
  alt (PureAndPoll a b) (OnlyEvent y) = PureAndPoll a (alt b $ Poll.sham y)
  alt (PureAndPoll a b) (OnlyPoll y) = PureAndPoll a (alt b y)
  alt (PureAndPoll a b) (PureAndPoll x y) = PureAndPoll (a <> x) (alt b y)

instance (Plus event, IsEvent event) => Plus (APoll event) where
  empty = OnlyPure []

-- | Merge together several polls. OnlyPure has the same functionality
-- | as `oneOf`, but it is faster and less prone to stack explosions.
merge :: forall a. Array (Poll a) → Poll a
merge a = case foldr go { l: [], m: [], r: [] } a of
  { l, m: [], r: [] } -> OnlyPure l
  { l:[], m, r: [] } -> OnlyEvent (Event.merge m)
  { l: [], m: [], r } -> OnlyPoll (Poll.merge r)
  -- todo: is it problematic that this is out of l2r order?
  { l, m, r } -> PureAndPoll l (Poll.sham (Event.merge m) <|> Poll.merge r)
  where

  go (OnlyPure q) { l, m, r } = { l: l <> q, m, r }
  go (OnlyEvent q) { l, m, r } = { l, m: m <> [ q ], r }
  go (OnlyPoll q) { l, m, r } = { l, m, r: r <> [ q ] }
  go (PureAndPoll x y) { l, m, r } = { l: l <> x, m, r: r <> [ y ] }

-- mergeMap is perfunctory here
mergeMap :: forall a b. (a -> Poll b) -> Array a → Poll b
mergeMap f a = merge (map f a)

-- | A poll where the answers are rigged by the nefarious `Event a`
sham :: forall event. IsEvent event => event ~> APoll event
sham = pollFromEvent

-- | Turn a function over events into a function over polls.
dredge :: forall a b event. IsEvent event => Alt event => Plus event => Apply event => (event a -> event b) -> APoll event a -> APoll event b
dredge f ea = pollFromPoll (Poll.dredge f (toPoll ea))

instance Poll.Pollable APoll Event Event where
  sample (PureAndPoll x y) ab = e <|> Poll.sample y ab
    where
    e = makeEvent \s -> s ab \f -> justMany (map f x)
  sample (OnlyEvent y) ab = y `EClass.sampleOnLeft` ab
  sample (OnlyPoll y) ab = Poll.sample y ab
  sample (OnlyPure x) ab = e
    where
    e = makeEvent \s -> s ab \f -> justMany (map f x)
else instance (IsEvent event, Apply event, Poll.Pollable APoll event event, Poll.Pollable Poll.APoll event event) => Poll.Pollable APoll event (APoll event) where
  sample = EClass.sampleOnRight
else instance (IsEvent event, Poll.Pollable Poll.APoll event event) => Poll.Pollable APoll event event where
  sample (PureAndPoll x y) ab = oneOf (map (ab <@> _) x <> [ Poll.sample y ab ])
  sample (OnlyPure x) ab = oneOf (map (ab <@> _) x)
  sample (OnlyEvent y) ab = y `EClass.sampleOnLeft` ab
  sample (OnlyPoll y) ab = Poll.sample y ab

-- | Switch `Poll`s based on an `Event`.
switcher :: forall event a. Apply event => Poll.Pollable APoll event event => Poll.Pollable Poll.APoll event event => IsEvent event => APoll event a -> event (APoll event a) -> APoll event a
switcher b e = EClass.keepLatest (pollFromOptimizedRep [ b ] e)

-- | Sample a `Poll` on some `Event` by providing a predicate function.
gateBy :: forall event p a. Poll.Pollable APoll event event => Filterable.Filterable event => (p -> a -> Boolean) -> APoll event p -> event a -> event a
gateBy f ps xs = Filterable.compact (Poll.sampleBy (\p x -> if f p x then Just x else Nothing) ps xs)

-- | Filter an `Event` by the boolean value of a `Poll`.
gate :: forall event a. Poll.Pollable APoll event event => Filterable.Filterable event => APoll event Boolean -> event a -> event a
gate = gateBy const

-- | Integrate with respect to some measure of time.
-- |
-- | OnlyPure function approximates the integral using the trapezium rule at the
-- | implicit sampling interval.
-- |
-- | The `Semiring` `a` should be a vector field over the field `t`. To represent
-- | this, the user should provide a _grate_ which lifts a multiplication
-- | function on `t` to a function on `a`. Simple examples where `t ~ a` can use
-- | the `integral'` function instead.
integral
  :: forall event a t
   . IsEvent event
  => Apply event
  => Poll.Pollable Poll.APoll event event
  => Poll.Pollable APoll event event
  => Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> APoll event t
  -> APoll event a
  -> APoll event a
integral g initial t b = pollFromPoll $ Poll.integral g initial (toPoll t) (toPoll b)

-- | Integrate with respect to some measure of time.
-- |
-- | OnlyPure function is a simpler version of `integral` where the function being
-- | integrated takes values in the same field used to represent time.
integral'
  :: forall event t
   . IsEvent event
  => Apply event
  => Poll.Pollable Poll.APoll event event
  => Poll.Pollable APoll event event
  => Field t
  => t
  -> APoll event t
  -> APoll event t
  -> APoll event t
integral' = integral (_ $ identity)

-- | Differentiate with respect to some measure of time.
-- |
-- | OnlyPure function approximates the derivative using a quotient of differences at the
-- | implicit sampling interval.
-- |
-- | The `Semiring` `a` should be a vector field over the field `t`. To represent
-- | this, the user should provide a grate which lifts a division
-- | function on `t` to a function on `a`. Simple examples where `t ~ a` can use
-- | the `derivative'` function.
derivative
  :: forall event a t
   . IsEvent event
  => Apply event
  => Poll.Pollable Poll.APoll event event
  => Poll.Pollable APoll event event
  => Field t
  => Ring a
  => (((a -> t) -> t) -> a)
  -> APoll event t
  -> APoll event a
  -> APoll event a
derivative g t b = pollFromPoll $ Poll.derivative g (toPoll t) (toPoll b)

-- | Differentiate with respect to some measure of time.
-- |
-- | OnlyPure function is a simpler version of `derivative` where the function being
-- | differentiated takes values in the same field used to represent time.
derivative'
  :: forall event t
   . IsEvent event
  => Apply event
  => Poll.Pollable Poll.APoll event event
  => Poll.Pollable APoll event event
  => Field t
  => APoll event t
  -> APoll event t
  -> APoll event t
derivative' = derivative (_ $ identity)

-- | Compute a fixed point
fixB :: forall event a. Poll.Pollable APoll event event => IsEvent event => a -> (APoll event a -> APoll event a) -> APoll event a
fixB a f =
  poll \s ->
    EClass.sampleOnRight
      ( EClass.fix \event ->
          let
            b = f (step a event)
          in
            Poll.sample_ b s
      )
      s

-- | Solve a first order differential equation of the form
-- |
-- | ```
-- | da/dt = f a
-- | ```
-- |
-- | by integrating once (specifying the initial conditions).
-- |
-- | For example, the exponential function with growth rate `⍺`:
-- |
-- | ```purescript
-- | exp = solve' 1.0 Time.seconds (⍺ * _)
-- | ```
solve
  :: forall t a
   . Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> Poll t
  -> (Poll a -> Poll a)
  -> Poll a
solve g a0 t f = fixB a0 \b -> integral g a0 t (f b)

-- | Solve a first order differential equation.
-- |
-- | OnlyPure function is a simpler version of `solve` where the function being
-- | integrated takes values in the same field used to represent time.
solve'
  :: forall a
   . Field a
  => a
  -> Poll a
  -> (Poll a -> Poll a)
  -> Poll a
solve' = solve (_ $ identity)

-- | Solve a second order differential equation of the form
-- |
-- | ```
-- | d^2a/dt^2 = f a (da/dt)
-- | ```
-- |
-- | by integrating twice (specifying the initial conditions).
-- |
-- | For example, an (damped) oscillator:
-- |
-- | ```purescript
-- | oscillate = solve2' 1.0 0.0 Time.seconds (\x dx -> -⍺ * x - δ * dx)
-- | ```
solve2
  :: forall t a
   . Field t
  => Semiring a
  => (((a -> t) -> t) -> a)
  -> a
  -> a
  -> Poll t
  -> (Poll a -> Poll a -> Poll a)
  -> Poll a
solve2 g a0 da0 t f =
  fixB a0 \b ->
    integral g a0 t
      ( fixB da0 \db ->
          integral g da0 t (f b db)
      )

-- | Solve a second order differential equation.
-- |
-- | OnlyPure function is a simpler version of `solve2` where the function being
-- | integrated takes values in the same field used to represent time.
solve2'
  :: forall a
   . Field a
  => a
  -> a
  -> Poll a
  -> (Poll a -> Poll a -> Poll a)
  -> Poll a
solve2' = solve2 (_ $ identity)

-- | Animate a `Poll` by providing a rendering function.
animate
  :: forall scene
   . APoll Event scene
  -> (scene -> Effect Unit)
  -> Effect (Effect Unit)
animate scene render = do
  { event, unsubscribe } <- animationFrame
  u2 <- subscribe (Poll.sample_ scene event) render
  pure do
    unsubscribe
    u2

-- | Turn an ST Ref into a poll
stRefToPoll :: STRef.STRef Global ~> Poll
stRefToPoll r = pollFromPoll (Poll.stRefToPoll r)

-- | Turn an ST Global into a poll
stToPoll :: ST Global ~> Poll
stToPoll r = pollFromPoll (Poll.stToPoll r)

filterMap
  :: forall event a b
   . Filterable.Compactable event
  => Poll.Pollable Poll.APoll event event
  => Poll.Pollable APoll event event
  => Filterable.Filterable event
  => Functor event
  => (a -> Maybe b)
  -> APoll event a
  -> APoll event b
filterMap f (PureAndPoll x y) = PureAndPoll (Filterable.filterMap f x) (Filterable.filterMap f y)
filterMap f (OnlyPure x) = OnlyPure (Filterable.filterMap f x)
filterMap f (OnlyEvent x) = OnlyEvent (Filterable.filterMap f x)
filterMap f (OnlyPoll y) = OnlyPoll (Filterable.filterMap f y)

partitionMap :: forall event a b c. Filterable.Filterable event => Poll.Pollable Poll.APoll event event => Poll.Pollable APoll event event => Filterable.Compactable event => Functor event => (a -> Either b c) -> APoll event a -> { left :: APoll event b, right :: APoll event c }
partitionMap f b = { left: filterMap (either Just (const Nothing)) fb, right: filterMap (either (const Nothing) Just) fb }
  where
  fb = f <$> b

instance (Functor event, Filterable.Filterable event, Filterable.Compactable event, Poll.Pollable Poll.APoll event event, Poll.Pollable APoll event event) => Filterable.Compactable (APoll event) where
  compact = filterMap identity
  separate = partitionMap identity

instance (Functor event, Filterable.Filterable event, Poll.Pollable Poll.APoll event event, Poll.Pollable APoll event event) => Filterable.Filterable (APoll event) where
  filterMap = filterMap
  filter = filterMap <<< maybeBool
  partitionMap = partitionMap
  partition p xs = do
    let o = partitionMap (eitherBool p) xs
    { no: o.left, yes: o.right }

sampleOnRight
  :: forall event a b
   . Apply event
  => Poll.Pollable APoll event event
  => Poll.Pollable Poll.APoll event event
  => IsEvent event
  => APoll event a
  -> APoll event (a -> b)
  -> APoll event b
sampleOnRight a b = pollFromPoll (toPoll a `EClass.sampleOnRight` toPoll b)

sampleOnLeft :: forall event a b. Apply event => Poll.Pollable Poll.APoll event event => Poll.Pollable APoll event event => IsEvent event => APoll event a -> APoll event (a -> b) -> APoll event b
sampleOnLeft a b = pollFromPoll (toPoll a `EClass.sampleOnLeft` toPoll b)

fix
  :: forall event a
   . Poll.Pollable APoll event event
  => Apply event
  => Poll.Pollable Poll.APoll event event
  => IsEvent event
  => (APoll event a -> APoll event a)
  -> APoll event a
fix f = pollFromPoll $ EClass.fix (dimap pollFromPoll toPoll f)

once :: forall event a. Apply event => Poll.Pollable APoll event event => Poll.Pollable Poll.APoll event event => IsEvent event => APoll event a -> APoll event a
once i = pollFromPoll $ EClass.once (toPoll i)

instance (IsEvent event, Apply event, Plus event, Poll.Pollable APoll event event, Poll.Pollable Poll.APoll event event) => IsEvent (APoll event) where
  sampleOnRight = sampleOnRight
  sampleOnLeft = sampleOnLeft
  keepLatest = keepLatest
  fix = fix
  once = once

type PollIO a = { poll :: Poll a, push :: a -> Effect Unit }
type PurePollIO a = { poll :: Poll a, push :: a -> ST Global Unit }

create
  :: forall a
   . ST Global (PollIO a)
create = do
  { event, push } <- Event.create
  { poll: p } <- rant (sham event)
  pure { poll: p, push }

createPure
  :: forall a
   . ST Global (PurePollIO a)
createPure = do
  { event, push } <- Event.createPure
  pure { poll: sham event, push }

mailbox
  :: forall a b
   . Ord a
  => ST Global { push :: { address :: a, payload :: b } -> Effect Unit, poll :: a -> Poll b }
mailbox = do
  { push, event } <- Event.mailbox
  pure { poll: map sham event, push }

-- Rant never emits the head, so we can just ignore it
rant
  :: forall a
   . Poll a
  -> ST Global { poll :: Poll a, unsubscribe :: ST Global Unit }
rant (PureAndPoll _ i) = do
  { poll: p, unsubscribe } <- Poll.rant i
  pure $ { poll: OnlyPoll p, unsubscribe }
-- todo: we should add or remove effect from some of these types
rant (OnlyEvent i) =
  ( unsafeCoerce
      :: Effect
           { poll :: APoll Event a
           , unsubscribe :: Effect Unit
           }
      -> ST Global
           { poll :: APoll Event a
           , unsubscribe :: ST Global Unit
           }
  ) do
    { event: e, unsubscribe } <- Event.memoize i
    pure $ { poll: OnlyEvent e, unsubscribe }
rant (OnlyPoll i) = do
  { poll: p, unsubscribe } <- Poll.rant i
  pure $ { poll: OnlyPoll p, unsubscribe }
rant (OnlyPure _) = pure $ { poll: empty, unsubscribe: pure unit }

deflect
  :: forall a
   . Poll a
  -> ST Global (Poll a)
deflect (PureAndPoll a b) = PureAndPoll a <$> Poll.deflect b
deflect (OnlyPoll b) = OnlyPoll <$> Poll.deflect b
deflect (OnlyEvent _) = pure $ OnlyPure []
deflect (OnlyPure a) = pure (OnlyPure a)

data KeepLatestOrder event a b
  = KeepLatestStart (APoll event a) (a -> b)
  | KeepLatestLast b

keepLatest
  :: forall event a
   . Filterable.Filterable event
  => Apply event
  => EClass.IsEvent event
  => Poll.Pollable Poll.APoll event event
  => Poll.Pollable APoll event event
  => APoll event (APoll event a)
  -> APoll event a
keepLatest (OnlyPure p) = fromMaybe empty (Array.last p)
keepLatest p = pollFromPoll $ EClass.keepLatest (toPoll (map toPoll p))