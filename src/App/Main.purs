module D3.Main where

import Prelude
import App.BarChart as BarChart
import App.Stateful as Stateful
import App.ThreeLittleCircles as ThreeLittleCircles
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: forall m. MonadAff m => Stories m
stories =
  Object.fromFoldable
    [ Tuple "" $ proxy $ ThreeLittleCircles.component
    , Tuple "Three little circles" $ proxy $ ThreeLittleCircles.component
    , Tuple "Bar charts|Simple bar chart examples" $ proxy $ BarChart.component
    , Tuple "Halogen|Stateful example" $ proxy $ Stateful.component
    ]

main :: Effect Unit
main =
  HA.runHalogenAff do
    HA.awaitBody
      >>= runStorybook
          { stories
          , logo: Just $ HH.text "Purescript D3 Bindings"
          }
