module D3.Svg
  ( SvgInput
  , createSvg
  ) where

import Prelude
import D3.Attributes (Attribute)
import D3.Attributes as Attributes
import D3.Event (Event)
import D3.Event as Event
import D3.Properties as Properties
import D3.Selection as Selection
import D3.Style (Style)
import D3.Style as Style
import D3.Types (Element(..), Margins, Selection)
import Effect (Effect)
import Halogen.Svg.Attributes.Transform as Transform

type SvgInput datum
  = { attributes :: Array (Attribute datum)
    , events :: Array (Event datum)
    , height :: Number
    , margins :: Margins
    , styles :: Array (Style datum)
    , width :: Number
    }

createSvg ::
  forall datum.
  String ->
  SvgInput datum ->
  Effect (Selection datum)
createSvg selector i =
  Selection.select selector
    >>= Selection.appendElement Svg
    >>= Attributes.setAttributes
        ( [ Attributes.height $ Properties.c $ i.height + i.margins.top + i.margins.bottom
          , Attributes.width $ Properties.c $ i.width + i.margins.left + i.margins.right
          ]
            <> i.attributes
        )
    >>= Event.setEvents i.events
    >>= Style.setStyles i.styles
    >>= Selection.appendElement Group
    >>= Attributes.setAttribute
        (Attributes.transform $ Properties.c [ Transform.Translate i.margins.left i.margins.top ])
