module FRP.Event.Random where

import Prelude

import Data.Op (Op(..))
import Effect (Effect)
import Effect.Random (random)
import Safe.Coerce (coerce)

withRandom :: forall a. Op (Effect Unit) { value :: a, random :: Number } -> Op (Effect Unit) a
withRandom = (coerce :: (_ -> a -> _ Unit) -> _ -> _) go
  where
  go f value = do
    r <- random
    f { random: r, value }
