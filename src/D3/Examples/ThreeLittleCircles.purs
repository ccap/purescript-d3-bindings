module D3.Examples.ThreeLittleCircles where

import Prelude
import D3 as D3
import Data.Array as Array
import Data.Int as Int
import Effect (Effect)

drawThreeCircles :: String -> Effect Unit
drawThreeCircles selector = do
  let
    svgHeight :: Number
    svgHeight = 150.0

    svgWidth :: Number
    svgWidth = 300.0

    margins :: D3.Margins
    margins = { top: 0.0, right: 0.0, left: 0.0, bottom: 0.0 }

    circleData :: Array Int
    circleData = [ 75, 150, 225 ]

    circleAttributes :: Array (D3.Attribute Int)
    circleAttributes =
      [ D3.radius $ D3.c 30.0
      , D3.centerX $ D3.d Int.toNumber
      , D3.centerY $ D3.c 75.0
      , D3.fill $ D3.c "green"
      ]
  svg <-
    D3.createSvg selector
      { attributes: []
      , events: []
      , height: svgHeight
      , margins
      , width: svgWidth
      , styles:
          [ D3.background $ D3.c "white"
          , D3.border $ D3.c "1px solid black"
          ]
      }
  _ <-
    D3.selectAllElementFromSelection D3.Circle svg
      >>= D3.bindData circleData
      >>= D3.enter
      >>= D3.appendElement D3.Circle
      >>= D3.setAttributes circleAttributes
  pure unit

drawMoreCircles :: String -> Effect Unit
drawMoreCircles selector = do
  let
    svgHeight :: Number
    svgHeight = 500.0

    svgWidth :: Number
    svgWidth = 500.0

    margins :: D3.Margins
    margins = { top: 0.0, right: 0.0, left: 0.0, bottom: 0.0 }

    chartData :: Array (Array Int)
    chartData =
      map
        (const $ Array.range 0 49)
        $ Array.range 0 49

    groupAttributes :: Array (D3.Attribute (Array Int))
    groupAttributes =
      [ D3.transform $ D3.di \_ i -> [ D3.Rotate (Int.toNumber i) 0.0 0.0 ]
      ]

    loopOnElements :: Array Int -> Int -> Array { index :: Int, d :: Int }
    loopOnElements d index = map { index, d: _ } d

    rgb :: { red :: Int, blue :: Int, green :: Int } -> String
    rgb { red, blue, green } = "rgb(" <> show red <> "," <> show green <> "," <> show blue <> ")"

    circleAttributes :: Array (D3.Attribute { index :: Int, d :: Int })
    circleAttributes =
      [ D3.radius $ D3.d (\{ d, index } -> 4.0 - ((Int.toNumber (d * index)) / 700.0))
      , D3.centerX $ D3.d (\x -> Int.toNumber $ x.d * 10)
      , D3.centerY $ D3.d (\{ index } -> Int.toNumber $ index * 10)
      , D3.fill $ D3.d \{ index, d } -> rgb { red: 5 + index * 5, blue: 255 - (d * 5), green: 0 }
      ]
  svg <-
    D3.createSvg selector
      { attributes: []
      , events: []
      , height: svgHeight
      , margins
      , width: svgWidth
      , styles:
          [ D3.background $ D3.c "black"
          , D3.border $ D3.c "1px solid black"
          ]
      }
  _ <-
    D3.selectAllElementFromSelection D3.Circle svg
      >>= D3.bindData chartData
      >>= D3.enter
      >>= D3.appendElement D3.Group
      >>= D3.setAttributes groupAttributes
      >>= D3.selectAllElementFromSelection D3.Circle
      >>= D3._mapSelectionLoopIndexed loopOnElements
      >>= D3.enter
      >>= D3.appendElement D3.Circle
      >>= D3.setAttributes circleAttributes
  pure unit
