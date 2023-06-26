module D3.Types
  ( DatumFn
  , Element(..)
  , EventBuilder
  , IndexedDatumFn
  , Margins
  , MouseEventConstantFn
  , MouseEventDatumFn
  , MouseEventIndexedDatumFn
  , PropFn(..)
  , PropFnMouseEvent(..)
  , PropertyBuilder(..)
  , Selection
  , Transition
  ) where

import Prelude
import Effect (Effect)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import data Selection :: Type -> Type

foreign import data Transition :: Type -> Type

data Element
  = Circle
  | Group
  | Line
  | Path
  | Rect
  | Svg
  | Text
  | TextSpan

data PropFn datum a
  = Constant a
  | Datum (DatumFn datum a)
  | IndexedDatum (IndexedDatumFn datum a)

type DatumFn datum a
  = datum -> a

type IndexedDatumFn datum a
  = datum -> Int -> a

data PropFnMouseEvent datum
  = MouseEventConstant (MouseEventConstantFn)
  | MouseEventDatum (MouseEventDatumFn datum)
  | MouseEventIndexedDatum (MouseEventIndexedDatumFn datum)

type MouseEventConstantFn
  = MouseEvent -> Effect Unit

type MouseEventDatumFn datum
  = MouseEvent -> datum -> Effect Unit

type MouseEventIndexedDatumFn datum
  = MouseEvent -> datum -> Int -> Effect Unit

type EventBuilder datum
  = PropFnMouseEvent datum

data PropertyBuilder datum
  = NumberAttribute (PropFn datum Number)
  | StringAttribute (PropFn datum String)

type Margins
  = { bottom :: Number
    , left :: Number
    , right :: Number
    , top :: Number
    }
