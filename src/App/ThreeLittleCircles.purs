module App.ThreeLittleCircles where

import Prelude
import D3.Examples.ThreeLittleCircles as D3
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State
  = {}

data Action
  = Init

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> {}
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render _state =
  HH.div_
    [ title "Three little circles"
    , HH.div [ HP.id "three-circles" ] []
    , title "A couple more circles"
    , HH.div [ HP.id "more-circles" ] []
    ]
  where
  title t = HH.h4_ [ HH.text t ]

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Init -> do
    H.liftEffect $ D3.drawThreeCircles "#three-circles"
    H.liftEffect $ D3.drawMoreCircles "#more-circles"
