module D3.Event
  ( Event
  , MouseEventT(..)
  , click
  , mouseenter
  , mouseleave
  , mousemove
  , mouseover
  , setEvent
  , setEvents
  ) where

import Prelude
import D3.Types (EventBuilder, PropFnMouseEvent(..))
import D3.Types as Types
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Effect (Effect)

foreign import _setEventConst ::
  forall datum.
  String ->
  Types.MouseEventConstantFn ->
  Types.Selection datum ->
  Effect (Types.Selection datum)

foreign import _setEvent ::
  forall datum.
  String ->
  Types.MouseEventDatumFn datum ->
  Types.Selection datum ->
  Effect (Types.Selection datum)

foreign import _setEventI ::
  forall datum.
  String ->
  Types.MouseEventIndexedDatumFn datum ->
  Types.Selection datum ->
  Effect (Types.Selection datum)

newtype MouseEventT
  = MouseEventT String

type Event datum
  = Tuple MouseEventT (EventBuilder datum)

type EventSetter datum
  = PropFnMouseEvent datum -> Event datum

type EventUnitConst datum
  = Types.MouseEventConstantFn -> Event datum

type EventUnit datum
  = Types.MouseEventDatumFn datum -> Event datum

type EventUnitIndexed datum
  = Types.MouseEventIndexedDatumFn datum -> Event datum

setEvent :: forall datum. Event datum -> Types.Selection datum -> Effect (Types.Selection datum)
setEvent (Tuple (MouseEventT attr) eventHandler) selection = consumePropFn eventHandler
  where
  consumePropFn :: PropFnMouseEvent datum -> Effect (Types.Selection datum)
  consumePropFn = case _ of
    MouseEventConstant toValue -> _setEventConst attr toValue selection
    MouseEventDatum toValue -> _setEvent attr toValue selection
    MouseEventIndexedDatum toValue -> _setEventI attr toValue selection

setEvents ::
  forall datum. Array (Event datum) -> Types.Selection datum -> Effect (Types.Selection datum)
setEvents styles selection = do
  _ <- Traversable.traverse (flip setEvent selection) styles
  pure selection

mouseover :: forall datum. EventSetter datum
mouseover = Tuple (MouseEventT "mouseover")

mouseleave :: forall datum. EventSetter datum
mouseleave = Tuple (MouseEventT "mouseleave")

mouseenter :: forall datum. EventSetter datum
mouseenter = Tuple (MouseEventT "mouseenter")

mousemove :: forall datum. EventSetter datum
mousemove = Tuple (MouseEventT "mousemove")

click :: forall datum. EventSetter datum
click = Tuple (MouseEventT "click")
