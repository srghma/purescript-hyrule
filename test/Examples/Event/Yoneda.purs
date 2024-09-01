module Test.Examples.Event.Yoneda where

import Prelude

import Control.Bind (bindFlipped)

type Yoneda f a = forall r. (a -> r) -> f r

proof_Yoneda_to :: forall f a. Yoneda f a -> f a
proof_Yoneda_to y = y identity

proof_Yoneda_from :: forall f a. Functor f => f a -> Yoneda f a
proof_Yoneda_from fa g = map g fa

type Curried f a = forall r. f (a -> r) -> f r

proof_Curried_to :: forall f a. Applicative f => Curried f a -> f a
proof_Curried_to c = c (pure identity)

proof_Curried_from :: forall f a. Applicative f => f a -> Curried f a
proof_Curried_from fa g = apply g fa

type Codensity :: forall k. (k -> Type) -> Type -> Type
type Codensity f a = forall r. (a -> f r) -> f r

proof_Codensity_to :: forall f a. Applicative f => Codensity f a -> f a
proof_Codensity_to c = c pure

proof_Codensity_from :: forall f a. Monad f => f a -> Codensity f a
proof_Codensity_from fa g = bindFlipped g fa

type Unknown f a = forall r. f (a -> f r) -> f r

proof_Unknown_to :: forall f a. Monad f => Unknown f a -> f a
proof_Unknown_to c = c (pure pure)

proof_Unknown_from :: forall f a. Monad f => f a -> Unknown f a
proof_Unknown_from fa fg = fa >>= \a -> fg >>= \g -> g a
