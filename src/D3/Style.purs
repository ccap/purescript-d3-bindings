module D3.Style
  ( Style
  , StyleT(..)
  , _numberStyle
  , _stringStyle
  , alignmentBaseline
  , background
  , border
  , color
  , cursor
  , fontSize
  , fontWeight
  , fontWidth
  , opacity
  , setStyle
  , setStyles
  , strokeWidth
  , textAnchor
  , whitespace
  ) where

import Prelude
import D3.Types (DatumFn, IndexedDatumFn, PropFn(..), PropertyBuilder(..), Selection)
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Effect (Effect)

foreign import _setStyleConst ::
  forall datum a. String -> a -> Selection datum -> Effect (Selection datum)

foreign import _setStyle ::
  forall datum a. String -> DatumFn datum a -> Selection datum -> Effect (Selection datum)

foreign import _setStyleI ::
  forall datum a. String -> IndexedDatumFn datum a -> Selection datum -> Effect (Selection datum)

newtype StyleT
  = StyleT String

type Style datum
  = Tuple StyleT (PropertyBuilder datum)

type StyleSetter datum a
  = PropFn datum a -> Style datum

type StyleSetterNumber datum
  = StyleSetter datum Number

type StyleSetterString datum
  = StyleSetter datum String

type StyleNumber_ datum
  = Number -> Style datum

type StyleNumber datum
  = DatumFn datum Number -> Style datum

type StyleNumberI datum
  = IndexedDatumFn datum Number -> Style datum

type StyleString_ datum
  = String -> Style datum

type StyleString datum
  = DatumFn datum String -> Style datum

type StyleStringI datum
  = IndexedDatumFn datum String -> Style datum

setStyle :: forall datum. Style datum -> Selection datum -> Effect (Selection datum)
setStyle (Tuple (StyleT attr) toValueFn) selection = case toValueFn of
  NumberAttribute attrFn -> consumePropFn attrFn
  StringAttribute attrFn -> consumePropFn attrFn
  where
  consumePropFn :: forall a. PropFn datum a -> Effect (Selection datum)
  consumePropFn = case _ of
    Constant value -> _setStyleConst attr value selection
    Datum toValue -> _setStyle attr toValue selection
    IndexedDatum toValue -> _setStyleI attr toValue selection

setStyles :: forall datum. Array (Style datum) -> Selection datum -> Effect (Selection datum)
setStyles styles selection = do
  _ <- Traversable.traverse (flip setStyle selection) styles
  pure selection

_numberStyle :: forall datum. String -> StyleSetter datum Number
_numberStyle style = Tuple (StyleT style) <<< NumberAttribute

_stringStyle :: forall datum. String -> StyleSetter datum String
_stringStyle style = Tuple (StyleT style) <<< StringAttribute

alignmentBaseline :: forall datum. StyleSetterString datum
alignmentBaseline = _stringStyle "alignment-baseline"

background :: forall datum. StyleSetter datum String
background = _stringStyle "background"

border :: forall datum. StyleSetter datum String
border = _stringStyle "border"

color :: forall datum. StyleSetter datum String
color = _stringStyle "color"

cursor :: forall datum. StyleSetter datum String
cursor = _stringStyle "cursor"

fontSize :: forall datum. StyleSetter datum String
fontSize = _stringStyle "font-size"

fontWeight :: forall datum. StyleSetter datum String
fontWeight = _stringStyle "font-weight"

fontWidth :: forall datum. StyleSetter datum String
fontWidth = _stringStyle "font-width"

opacity :: forall datum. StyleSetter datum Number
opacity = _numberStyle "opacity"

strokeWidth :: forall datum. StyleSetter datum String
strokeWidth = _stringStyle "stroke-width"

textAnchor :: forall datum. StyleSetter datum String
textAnchor = _stringStyle "text-anchor"

whitespace :: forall datum. StyleSetter datum String
whitespace = _stringStyle "white-space"
