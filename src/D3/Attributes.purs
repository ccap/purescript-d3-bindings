module D3.Attributes
  ( AttrT(..)
  , Attribute
  , _numberAttr
  , _stringAttr
  , centerX
  , centerY
  , classAttr
  , fill
  , height
  , radius
  , setAttribute
  , setAttributes
  , stroke
  , transform
  , width
  , x
  , y
  ) where

import Prelude
import D3.Properties as Properties
import D3.Types (PropFn(..), PropertyBuilder(..))
import D3.Types as Types
import Data.Array as Array
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen.Svg.Attributes.Transform (Transform, printTransform) as HSA

foreign import _setAttributeConst ::
  forall datum a. String -> a -> Types.Selection datum -> Effect (Types.Selection datum)

foreign import _setAttribute ::
  forall datum a.
  String ->
  Types.DatumFn datum a ->
  Types.Selection datum ->
  Effect (Types.Selection datum)

foreign import _setAttributeI ::
  forall datum a.
  String ->
  Types.IndexedDatumFn datum a ->
  Types.Selection datum ->
  Effect (Types.Selection datum)

newtype AttrT
  = AttrT String

type Attribute datum
  = Tuple AttrT (PropertyBuilder datum)

setAttribute ::
  forall datum. Attribute datum -> Types.Selection datum -> Effect (Types.Selection datum)
setAttribute (Tuple (AttrT attr) toValueFn) selection = case toValueFn of
  NumberAttribute attrFn -> consumePropFn attrFn
  StringAttribute attrFn -> consumePropFn attrFn
  where
  consumePropFn :: forall a. PropFn datum a -> Effect (Types.Selection datum)
  consumePropFn = case _ of
    Constant value -> _setAttributeConst attr value selection
    Datum toValue -> _setAttribute attr toValue selection
    IndexedDatum toValue -> _setAttributeI attr toValue selection

setAttributes ::
  forall datum. Array (Attribute datum) -> Types.Selection datum -> Effect (Types.Selection datum)
setAttributes attrs selection = do
  _ <- Traversable.traverse (flip setAttribute selection) attrs
  pure selection

type AttributeSetter datum a
  = PropFn datum a -> Attribute datum

type AttributeNumber_ datum
  = Number -> Attribute datum

type AttributeNumber datum
  = Types.DatumFn datum Number -> Attribute datum

type AttributeNumberI datum
  = Types.IndexedDatumFn datum Number -> Attribute datum

type AttributeString_ datum
  = String -> Attribute datum

type AttributeString datum
  = Types.DatumFn datum String -> Attribute datum

type AttributeStringI datum
  = Types.IndexedDatumFn datum String -> Attribute datum

_numberAttr :: forall datum. String -> AttributeSetter datum Number
_numberAttr attr = Tuple (AttrT attr) <<< NumberAttribute

_stringAttr :: forall datum. String -> AttributeSetter datum String
_stringAttr attr = Tuple (AttrT attr) <<< StringAttribute

-- New pattern, it's easy enough to switch in the caller so don't create
-- separate functions for each PropFn 
stroke :: forall datum. AttributeSetter datum String
stroke = _stringAttr "stroke"

radius :: forall datum. AttributeSetter datum Number
radius = _numberAttr "r"

centerX :: forall datum. AttributeSetter datum Number
centerX = _numberAttr "cx"

centerY :: forall datum. AttributeSetter datum Number
centerY = _numberAttr "cy"

classAttr :: forall datum. AttributeSetter datum String
classAttr = _stringAttr "class"

height :: forall datum. AttributeSetter datum Number
height = _numberAttr "height"

width :: forall datum. AttributeSetter datum Number
width = _numberAttr "width"

x :: forall datum. AttributeSetter datum Number
x = _numberAttr "x"

y :: forall datum. AttributeSetter datum Number
y = _numberAttr "y"

fill :: forall datum. AttributeSetter datum String
fill = _stringAttr "fill"

transform :: forall datum. AttributeSetter datum (Array HSA.Transform)
transform fn =
  _stringAttr
    "transform"
    (Properties.overPropFn fn (Array.intercalate " " <<< map HSA.printTransform))
