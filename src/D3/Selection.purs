module D3.Selection
  ( Join
  , _append
  , _bindData
  , _call
  , _empty
  , _enter
  , _filter
  , _getData
  , _mapSelectionLoop
  , _mapSelectionLoopIndexed
  , _node
  , _remove
  , _select
  , _selectAll
  , _selectAllFromSelection
  , _selectFromSelection
  , _transition
  , appendElement
  , bindData
  , elementToString
  , emptyJoin
  , enter
  , filter
  , partition
  , select
  , selectAllById
  , selectAllElementFromSelection
  , selectAllFromSelection
  , selectAllTextFromSelection
  , selectById
  , selectElementById
  , selectElementFromSelection
  , transitionToSelection
  ) where

import Prelude
import D3.Types (Element(..), Selection, Transition)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element as Element

foreign import _append :: forall a. Selection a -> String -> Effect (Selection a)

foreign import _bindData ::
  forall oldDatum datum. Array datum -> Selection oldDatum -> Effect (Selection datum)

-- Call a function exactly once
foreign import _call ::
  forall datum. Selection datum -> Effect (Selection datum) -> Effect (Selection datum)

foreign import _empty :: forall datum. Selection datum -> Effect Boolean

foreign import _enter :: forall datum. Selection datum -> Effect (Selection datum)

foreign import _filter ::
  forall datum. (datum -> Boolean) -> Selection datum -> Effect (Selection datum)

foreign import _getData :: forall datum. Selection datum -> Effect (Array datum)

foreign import _mapSelectionLoop ::
  forall a b f. (f a -> f b) -> Selection (f a) -> Effect (Selection b)

foreign import _mapSelectionLoopIndexed ::
  forall a b f. (f a -> Int -> f b) -> Selection (f a) -> Effect (Selection b)

foreign import _remove :: forall a. Selection a -> Effect (Selection Unit)

foreign import _transition :: forall datum. Selection datum -> Effect (Transition datum)

foreign import _select :: forall a. String -> Effect (Selection a)

foreign import _selectAll :: forall a. String -> Effect (Selection a)

foreign import _selectAllFromSelection ::
  forall datum result. String -> Selection datum -> Effect (Selection result)

foreign import _selectFromSelection ::
  forall datum result. String -> Selection datum -> Effect (Selection result)

foreign import _node ::
  forall datum.
  Selection datum ->
  (Element.Element -> Maybe Element.Element) ->
  Maybe Element.Element ->
  Effect (Maybe Element.Element)

appendElement :: forall a. Element -> Selection a -> Effect (Selection a)
appendElement element selection = _append selection (elementToString element)

bindData :: forall oldDatum datum. Array datum -> Selection oldDatum -> Effect (Selection datum)
bindData = _bindData

enter :: forall datum. Selection datum -> Effect (Selection datum)
enter = _enter

elementToString :: Element -> String
elementToString = case _ of
  Circle -> "circle"
  Group -> "g"
  Line -> "line"
  Path -> "path"
  Rect -> "rect"
  Svg -> "svg"
  Text -> "text"
  TextSpan -> "tspan"

transitionToSelection :: forall datum. Transition datum -> Selection datum
transitionToSelection = unsafeCoerce

select :: forall a. String -> Effect (Selection a)
select = _select

filter :: forall datum. (datum -> Boolean) -> Selection datum -> Effect (Selection datum)
filter = _filter

partition ::
  forall datum.
  (datum -> Boolean) ->
  Selection datum ->
  Effect { yes :: Selection datum, no :: Selection datum }
partition cond selection = do
  yes <- _filter cond selection
  no <- _filter (not <<< cond) selection
  pure { yes, no }

selectElementById ::
  forall datum.
  String ->
  Effect { selection :: Selection datum, element :: Maybe Element.Element }
selectElementById id = do
  selection <- selectById id
  element <- _node selection Just Nothing
  pure { element, selection }

selectById :: forall datum. String -> Effect (Selection datum)
selectById s = _select ("#" <> s)

selectAllById :: forall datum. String -> Effect (Selection datum)
selectAllById s = _selectAll ("#" <> s)

selectElementFromSelection :: forall datum. Element -> Selection datum -> Effect (Selection datum)
selectElementFromSelection element = _selectFromSelection (elementToString element)

selectAllElementFromSelection :: forall datum. Element -> Selection datum -> Effect (Selection datum)
selectAllElementFromSelection element = _selectAllFromSelection (elementToString element)

selectAllTextFromSelection :: forall datum. Selection datum -> Effect (Selection datum)
selectAllTextFromSelection = selectAllElementFromSelection Text

selectAllFromSelection :: forall datum. String -> Selection datum -> Effect (Selection datum)
selectAllFromSelection = _selectAllFromSelection

-- Join seems to be a more streamlined pattern for managing mount/unmounting elements 
-- TODO: Flesh out.
type Join datum
  = { enter :: Selection datum -> Effect (Selection datum)
    , update :: Selection datum -> Effect (Selection datum)
    , exit :: Selection datum -> Effect (Selection datum)
    }

emptyJoin :: forall datum. Join datum
emptyJoin =
  { enter: pure <<< identity
  , update: pure <<< identity
  , exit: pure <<< identity
  }
