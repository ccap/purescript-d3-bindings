module App.Stateful where

import Prelude
import D3 as D3
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.UIEvent.MouseEvent (MouseEvent)

-- TODO (DRS): join with enter/exit/update is by far simpler to write than other ways of handling it.
-- Implement standard FFI w/ at least a simple case here + flesh out this example more.
foreign import _join :: forall datum. D3.Selection datum -> Effect (D3.Selection datum)

type State
  = { coords :: Maybe { x :: Number, y :: Number } }

type Coords
  = { x :: Number, y :: Number }

foreign import _offsetCoords :: MouseEvent -> Effect Coords

type Output
  = Unit

data Action
  = Init
  | HandleMouseMove MouseEvent

component :: forall q i m. MonadEffect m => H.Component q i Output m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { coords: Nothing }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div_
    [ HH.text $ "Hello my state is " <> (show _state)
    , HH.br_
    , HH.div [ HP.id svgContainerId ] []
    ]

type HalogenM m
  = H.HalogenM State Action () Output m

handleAction :: forall m. MonadEffect m => Action → HalogenM m Unit
handleAction = case _ of
  HandleMouseMove e -> do
    offsetCoords <- H.liftEffect $ _offsetCoords e
    newState <- H.modify _ { coords = Just offsetCoords }
    H.liftEffect $ draw newState
  Init -> do
    state <- H.get
    { emitter, listener } <- H.liftEffect HS.create
    void $ H.subscribe emitter
    let
      events :: Array (D3.Event Unit)
      events =
        [ D3.mousemove
            $ D3.cMe \event ->
                HS.notify listener $ HandleMouseMove event
        ]
    void
      $ H.liftEffect
      $ D3.createSvg
          ("#" <> svgContainerId)
          { attributes: []
          , events
          , height: 500.0
          , margins: { top: 0.0, right: 0.0, left: 0.0, bottom: 0.0 }
          , styles: [ D3.border $ D3.c "1px solid black" ]
          , width: 500.0
          }
    H.liftEffect $ draw state

draw :: State -> Effect Unit
draw state = do
  void $ D3.selectById svgContainerId
    >>= D3.selectAllElementFromSelection D3.Svg
    >>= D3.selectElementFromSelection D3.Group
    >>= D3.selectAllElementFromSelection D3.Rect
    >>= D3.bindData (Array.fromFoldable state.coords)
    >>= _join

svgContainerId = "stateful-example" :: String
