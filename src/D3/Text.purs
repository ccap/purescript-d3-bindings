module D3.Text
  ( text
  , textI
  , text_
  ) where

import Prelude
import D3.Types (DatumFn, IndexedDatumFn, PropFn(..), Selection)
import Effect (Effect)

foreign import _setTextConst :: forall datum. String -> Selection datum -> Effect (Selection datum)

foreign import _setText ::
  forall datum. DatumFn datum String -> Selection datum -> Effect (Selection datum)

foreign import _setTextI ::
  forall datum. IndexedDatumFn datum String -> Selection datum -> Effect (Selection datum)

_text :: forall datum. PropFn datum String -> Selection datum -> Effect (Selection datum)
_text propFn selection = case propFn of
  Constant value -> _setTextConst value selection
  Datum toValue -> _setText toValue selection
  IndexedDatum toValue -> _setTextI toValue selection

text_ :: forall datum. String -> Selection datum -> Effect (Selection datum)
text_ = _text <<< Constant

text :: forall datum. DatumFn datum String -> Selection datum -> Effect (Selection datum)
text = _text <<< Datum

textI :: forall datum. IndexedDatumFn datum String -> Selection datum -> Effect (Selection datum)
textI = _text <<< IndexedDatum
