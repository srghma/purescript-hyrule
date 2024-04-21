module FRP.Event.AnimationFrame
  ( animationFrame'
  , animationFrame
  ) where

import Prelude

import Data.Op (Op(..))
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, makeEventE)
import Safe.Coerce (coerce)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

-- | Create an event which fires every frame (using `requestAnimationFrame`).
animationFrame'
  :: forall a
   . (Op (Effect Unit) a -> Op (Effect Unit) Unit)
  -> Effect
       { event :: Event a
       , unsubscribe :: Effect Unit
       }
animationFrame' f = makeEventE \k -> do
  w <- window
  cancelled <- Ref.new false
  let
    loop = void do
      w # requestAnimationFrame do
        (coerce :: _ -> _ -> _ -> _ Unit) f k unit
        unlessM (Ref.read cancelled) loop
  loop
  pure (Ref.write true cancelled)

animationFrame
  :: Effect
       { event :: Event Unit
       , unsubscribe :: Effect Unit
       }
animationFrame = animationFrame' identity