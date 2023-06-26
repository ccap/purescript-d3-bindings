module App.BarChart where

import Prelude
import D3.Examples.BarChart as D3
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
    [ HH.div [ HP.id "bar-chart-percent-stacked" ] []
    , HH.div [ HP.id "bar-chart-stacked-vertical" ] []
    , HH.div [ HP.id "bar-chart-grouped" ] []
    , HH.br_
    , HH.div [ HP.id "bar-chart-vertical" ] []
    , HH.div [ HP.id "bar-chart-horizontal" ] []
    ]

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Init -> do
    H.liftEffect $ D3.verticalBarChart "#bar-chart-vertical"
    H.liftEffect $ D3.horizontalBarChart "#bar-chart-horizontal"
    H.liftEffect $ D3.verticalStackedBarChart "#bar-chart-stacked-vertical"
    H.liftEffect $ D3.verticalGroupedBarChart "#bar-chart-stacked-vertical"
    H.liftEffect $ D3.stackedPercentageBarChart "#bar-chart-percent-stacked"
