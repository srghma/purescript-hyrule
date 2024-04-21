module FRP.Event.Multiplex
  ( withMultiplexing
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Op (Op(..))
import Effect (Effect)

withMultiplexing :: forall a b. Array (Op (Effect Unit) b -> Op (Effect Unit) a) -> Op (Effect Unit) b -> Op (Effect Unit) a
withMultiplexing l op = Op \a ->
  for_ l \f -> let Op x = f op in x a