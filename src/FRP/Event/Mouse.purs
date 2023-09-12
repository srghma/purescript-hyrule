module FRP.Event.Mouse
  ( Mouse
  , getMouse
  , disposeMouse
  , down'
  , down
  , up'
  , up
  , withPosition
  , withButtons
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Op (Op(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, makeEventE)
import Safe.Coerce (coerce)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.MouseEvent (button, clientX, clientY, fromEvent)

-- | A handle for creating events from the mouse position and buttons.
newtype Mouse = Mouse
  { position :: Ref.Ref (Maybe { x :: Int, y :: Int })
  , buttons :: Ref.Ref (Set.Set Int)
  , dispose :: Effect Unit
  }

-- | Get a handle for working with the mouse.
getMouse :: Effect Mouse
getMouse = do
  position <- Ref.new Nothing
  buttons <- Ref.new Set.empty
  target <- toEventTarget <$> window
  mouseMoveListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      Ref.write (Just { x: clientX me, y: clientY me }) position
  mouseDownListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      Ref.modify (Set.insert (button me)) buttons
  mouseUpListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      Ref.modify (Set.delete (button me)) buttons
  addEventListener (wrap "mousemove") mouseMoveListener false target
  addEventListener (wrap "mousedown") mouseDownListener false target
  addEventListener (wrap "mouseup") mouseUpListener false target
  let
    dispose = do
      removeEventListener (wrap "mousemove") mouseMoveListener false target
      removeEventListener (wrap "mousedown") mouseDownListener false target
      removeEventListener (wrap "mouseup") mouseUpListener false target
  pure (Mouse { position, buttons, dispose })

disposeMouse :: Mouse -> Effect Unit
disposeMouse (Mouse { dispose }) = dispose

-- | Create an `Event` which fires when a mouse button is pressed
down'
  :: forall a
   . (Op (Effect Unit) a -> Op (Effect Unit) Int)
  -> Effect
       { event :: Event a
       , unsubscribe :: Effect Unit
       }
down' f = makeEventE \k -> do
  target <- toEventTarget <$> window
  mouseDownListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      (coerce :: _ -> _ -> _ -> _ Unit) f k (button me)
  addEventListener (wrap "mousedown") mouseDownListener false target
  pure (removeEventListener (wrap "mousedown") mouseDownListener false target)

down
  :: Effect
       { event :: Event Int
       , unsubscribe :: Effect Unit
       }
down = down' identity

-- | Create an `Event` which fires when a mouse button is released
up'
  :: forall a
   . (Op (Effect Unit) a -> Op (Effect Unit) Int)
  -> Effect
       { event :: Event a
       , unsubscribe :: Effect Unit
       }
up' f = makeEventE \k -> do
  target <- toEventTarget <$> window
  mouseUpListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      (coerce :: _ -> _ -> _ -> _ Unit) f k (button me)
  addEventListener (wrap "mouseup") mouseUpListener false target
  pure (removeEventListener (wrap "mouseup") mouseUpListener false target)

up
  :: Effect
       { event :: Event Int
       , unsubscribe :: Effect Unit
       }
up = up' identity

-- | Create an event which also returns the current mouse position.
withPosition
  :: forall a
   . Mouse
  -> Op (Effect Unit) { value :: a, pos :: Maybe { x :: Int, y :: Int } }
  -> Op (Effect Unit) a
withPosition (Mouse { position }) = (coerce :: (_ -> a -> _ Unit) -> _ -> _) go
  where
  go f value = do
    pos <- Ref.read position
    f { value, pos }

-- | Create an event which also returns the current mouse buttons.
withButtons
  :: forall a
   . Mouse
  -> Op (Effect Unit) { value :: a, buttons :: Set.Set Int }
  -> Op (Effect Unit) a
withButtons (Mouse { buttons }) = (coerce :: (_ -> a -> _ Unit) -> _ -> _) go
  where
  go f value = do
    buttonsValue <- Ref.read buttons
    f { value, buttons: buttonsValue }
