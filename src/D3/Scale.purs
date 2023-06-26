module D3.Scale
  ( class IsScale
  , Axis
  , AxisConfig
  , AxisLocation(..)
  , Scale(..)
  , ScaleBand
  , ScaleBandInputConfig
  , ScaleGeneric
  , ScaleI
  , ScaleLinear
  , ScaleLinearInputConfig
  , ScaleNumericIRow
  , ScaleStringIRow
  , _attachAxis
  , _attachScaleBottom
  , _attachScaleLeft
  , _axisBottom
  , _axisLeft
  , _bandwidth
  , _scaleBandValue
  , _scaleLinear
  , _setPadding
  , _setTickFormat
  , _setTickSize
  , _setTickValues
  , _setTicks
  , attachAxis
  , attachScale
  , bandToScale
  , bandwidth
  , createAxis
  , createScaleLinear
  , defaultScaleBandInput
  , defaultScaleLinearInput
  , emptyAxisConfig
  , emptyAxisConfigNumber
  , emptyAxisConfigString
  , emptyScaleBandInput
  , genericScale
  , linearToScale
  , scale
  , scaleBandCreate
  , scaleLinearCreate
  , setAxisParams
  ) where

import Prelude
import D3.Types (Selection)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Record (merge)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data ScaleLinear :: Type

foreign import data ScaleBand :: Type

foreign import data ScaleGeneric :: Type

foreign import _attachScaleBottom :: forall datum domain. ScaleGeneric -> Selection datum -> Effect (Selection domain)

foreign import _attachScaleLeft :: forall datum domain. ScaleGeneric -> Selection datum -> Effect (Selection domain)

foreign import _bandwidth :: ScaleBand -> Number

foreign import _scaleBand :: { domain :: Array String, range :: Array Number } -> Effect ScaleBand

foreign import _scaleBandValue :: ScaleBand -> String -> Number

foreign import _scaleLinear :: { domain :: Array Number, range :: Array Number } -> Effect ScaleLinear

foreign import _scaleLinearValue :: ScaleLinear -> Number -> Number

foreign import data Axis :: Type -> Type

foreign import _attachAxis :: forall datum domain. Axis domain -> Selection datum -> Effect (Selection domain)

foreign import _axisLeft :: forall domain scale. scale -> Effect (Axis domain)

foreign import _axisBottom :: forall domain scale. scale -> Effect (Axis domain)

class IsScale scale domain where
  scale :: scale -> domain -> Number

instance isScaleLinear :: IsScale ScaleLinear Number where
  scale = _scaleLinearValue

instance isScaleBand :: IsScale ScaleBand String where
  scale = _scaleBandValue

attachAxis :: forall datum domain. Axis domain -> Selection datum -> Effect (Selection domain)
attachAxis = _attachAxis

createAxis ::
  forall scale domain.
  IsScale scale domain =>
  scale ->
  AxisLocation ->
  AxisConfig domain ->
  Effect (Axis domain)
createAxis scale location config = do
  axis <- case location of
    AxisBottom -> _axisBottom scale
    AxisLeft -> _axisLeft scale
  setAxisParams config axis

emptyAxisConfigNumber :: AxisConfig Number
emptyAxisConfigNumber = emptyAxisConfig

emptyAxisConfigString :: AxisConfig String
emptyAxisConfigString = emptyAxisConfig

emptyAxisConfig :: forall domain. AxisConfig domain
emptyAxisConfig =
  { tickFormat: Nothing
  , tickSize: Nothing
  , tickValues: Nothing
  , ticks: Nothing
  }

setAxisParams :: forall domain. AxisConfig domain -> (Axis domain) -> Effect (Axis domain)
setAxisParams i axis =
  setParam _setTicks i.ticks axis
    >>= setParam _setTickFormat i.tickFormat
    >>= setParam _setTickSize i.tickSize
    >>= setParam _setTickValues i.tickValues
  where
  setParam ::
    forall param.
    (Axis domain -> param -> Effect (Axis domain)) ->
    Maybe param ->
    (Axis domain) ->
    Effect (Axis domain)
  setParam set param a = maybe (pure a) (set a) param

type AxisConfig domain
  = { ticks :: Maybe Int
    , tickFormat :: Maybe (domain -> Int -> String)
    , tickSize :: Maybe Number
    , tickValues :: Maybe (Array domain)
    }

type ScaleI domain r
  = ( domain :: Array domain, range :: Array Number | r )

type ScaleNumericIRow r
  = ScaleI Number r

type ScaleStringIRow r
  = ScaleI String r

type ScaleLinearInputConfig
  = Record
      ( ScaleNumericIRow
          + ()
      )

type ScaleBandInputConfig
  = Record
      ( ScaleStringIRow
          + ( padding :: Maybe Number
          )
      )

emptyScaleBandInput :: ScaleBandInputConfig
emptyScaleBandInput =
  { domain: []
  , range: []
  , padding: Nothing
  }

defaultScaleBandInput :: Record (ScaleStringIRow ()) -> ScaleBandInputConfig
defaultScaleBandInput = merge { padding: Nothing }

defaultScaleLinearInput :: Record (ScaleNumericIRow ()) -> ScaleLinearInputConfig
defaultScaleLinearInput = merge {}

foreign import _setPadding :: ScaleBand -> Number -> Effect ScaleBand

foreign import _setTicks :: forall domain. (Axis domain) -> Int -> Effect (Axis domain)

foreign import _setTickValues :: forall domain. (Axis domain) -> Array domain -> Effect (Axis domain)

foreign import _setTickSize :: forall domain. (Axis domain) -> Number -> Effect (Axis domain)

foreign import _setTickFormat :: forall domain. (Axis domain) -> (domain -> Int -> String) -> Effect (Axis domain)

createScaleLinear :: ScaleLinearInputConfig -> Effect ScaleLinear
createScaleLinear i = _scaleLinear { domain: i.domain, range: i.range }

scaleLinearCreate :: ScaleLinearInputConfig -> Effect ScaleLinear
scaleLinearCreate = createScaleLinear

scaleBandCreate :: ScaleBandInputConfig -> Effect ScaleBand
scaleBandCreate i = do
  scale <- _scaleBand { domain: i.domain, range: i.range }
  setParam _setPadding i.padding scale
  where
  setParam ::
    forall param.
    (ScaleBand -> param -> Effect ScaleBand) ->
    Maybe param ->
    ScaleBand ->
    Effect ScaleBand
  setParam set param s = maybe (pure s) (set s) param

data Scale
  = Band ScaleBand
  | Linear ScaleLinear

data AxisLocation
  = AxisBottom
  | AxisLeft

attachScale :: forall datum domain. Scale -> AxisLocation -> Selection datum -> Effect (Selection domain)
attachScale scale location selection = case location of
  AxisBottom -> _attachScaleBottom (genericScale scale) selection
  AxisLeft -> _attachScaleLeft (genericScale scale) selection

bandToScale :: ScaleBand -> Scale
bandToScale = Band

bandwidth :: ScaleBand -> Number
bandwidth = _bandwidth

genericScale :: Scale -> ScaleGeneric
genericScale = case _ of
  Band s -> unsafeCoerce s
  Linear s -> unsafeCoerce s

linearToScale :: ScaleLinear -> Scale
linearToScale = Linear
