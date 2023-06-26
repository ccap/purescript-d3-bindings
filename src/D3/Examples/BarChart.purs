module D3.Examples.BarChart where

import Prelude
import D3 (AxisLocation(..), Element(..))
import D3 as D3
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)

data TestColor
  = Red
  | Blue
  | Green

instance showTestColor :: Show TestColor where
  show = case _ of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"

derive instance eqTestColor :: Eq TestColor

type BarChartData
  = { label :: String, value :: Number, color :: TestColor }

type BarChartDataStacked
  = { label :: String, red :: Number, blue :: Number, green :: Number }

barChartDataStacked :: Array BarChartDataStacked
barChartDataStacked =
  map
    ( \group ->
        let
          { label } = NonEmptyArray.head group

          sumColor color =
            Foldable.sum $ map _.value $ Array.filter (eq color <<< _.color)
              $ NonEmptyArray.toArray group
        in
          { label
          , green: sumColor Green
          , red: sumColor Red
          , blue: sumColor Blue
          }
    )
    $ Array.groupBy (eq `on` _.label) barChartData

barChartData :: Array BarChartData
barChartData =
  [ { label: "A", value: 1.0, color: Blue }
  , { label: "A", value: 1.0, color: Green }
  , { label: "A", value: 1.0, color: Red }
  , { label: "B", value: 1.0, color: Red }
  , { label: "B", value: 2.0, color: Blue }
  , { label: "B", value: 2.0, color: Green }
  , { label: "C", value: 3.0, color: Blue }
  , { label: "C", value: 3.0, color: Green }
  , { label: "D", value: 4.0, color: Red }
  ]

verticalBarChart :: String -> Effect Unit
verticalBarChart selector = do
  let
    svgHeight = 200.0

    svgWidth = 200.0

    margins = { top: 20.0, right: 20.0, left: 50.0, bottom: 50.0 }

    toLabel { label, color } = label <> " (" <> show color <> ")"
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
  xAxis <- scaleBand (map toLabel barChartData) [ 0.0, svgWidth ]
  _ <-
    D3.appendElement Group svg
      >>= D3.setAttribute
          (D3.transform $ D3.c [ D3.Translate 0.0 svgHeight ])
      >>= D3.attachScale (D3.bandToScale xAxis) AxisBottom
      >>= D3.selectAllElementFromSelection Text
      >>= D3.setAttributes
          [ D3.transform $ D3.c [ D3.Rotate (-45.0) 0.0 0.0 ]
          ]
      >>= D3.setStyles
          [ D3.textAnchor $ D3.c "end"
          ]
  yAxis <- D3.scaleLinearCreate { domain: [ 0.0, 4.0 ], range: [ svgHeight, 0.0 ] }
  _ <-
    D3.appendElement Group svg
      >>= D3.attachScale (D3.linearToScale yAxis) AxisLeft
  _ <-
    D3._selectAllFromSelection "mybar" svg
      >>= D3.bindData barChartData
      >>= D3.enter
      >>= D3.appendElement Rect
      >>= D3.setAttributes
          [ D3.x $ D3.d $ D3.scale xAxis <<< toLabel
          , D3.y $ D3.d $ D3.scale yAxis <<< _.value
          , D3.width $ D3.c $ D3.bandwidth xAxis
          , D3.height $ D3.d \d -> svgHeight - D3.scale yAxis d.value
          , D3.fill $ D3.d $ show <<< _.color
          ]
  pure unit

horizontalBarChart :: String -> Effect Unit
horizontalBarChart selector = do
  let
    svgHeight = 200.0

    svgWidth = 200.0

    margins = { top: 20.0, right: 20.0, left: 50.0, bottom: 50.0 }
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
  yAxis <- scaleBand (map _.label barChartData) [ 0.0, svgWidth ]
  _ <- D3.appendElement Group svg >>= D3.attachScale (D3.bandToScale yAxis) AxisLeft
  xAxis <- D3.scaleLinearCreate { domain: [ 0.0, 4.0 ], range: [ svgHeight, 0.0 ] }
  _ <-
    D3.appendElement Group svg
      >>= D3.attachScale (D3.linearToScale xAxis) AxisBottom
      >>= D3.setAttribute (D3.transform $ D3.c [ D3.Translate 0.0 svgHeight ])
  _ <-
    D3._selectAllFromSelection "mybar" svg
      >>= D3.bindData barChartData
      >>= D3.enter
      >>= D3.appendElement Rect
      >>= D3.setAttributes
          [ D3.fill $ D3.d $ show <<< _.color
          , D3.height $ D3.c $ D3.bandwidth yAxis
          , D3.width $ D3.d (\d -> svgWidth - D3.scale xAxis d.value)
          , D3.x $ D3.c 0.0
          , D3.y $ D3.d $ D3.scale yAxis <<< _.label
          ]
  pure unit

verticalStackedBarChart :: String -> Effect Unit
verticalStackedBarChart selector = do
  let
    svgHeight = 300.0

    svgWidth = 300.0

    margins = { top: 20.0, right: 20.0, left: 50.0, bottom: 50.0 }

    groups = Array.sort $ Array.nub $ map _.label barChartData

    subgroups = Array.sort $ Array.nub $ [ "red", "blue", "green" ]
  stackBarChartData <- D3._stackData subgroups barChartDataStacked
  let
    maxTick =
      fromMaybe 0.0
        $ Foldable.maximum
        $ map (Foldable.sum <<< map _.value)
        $ Array.groupBy (\a b -> a.label == b.label) barChartData
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
  xAxis <- scaleBand groups [ 0.0, svgWidth ]
  _ <-
    D3.appendElement Group svg
      >>= D3.setAttribute
          (D3.transform $ D3.c [ D3.Translate 0.0 svgHeight ])
      >>= D3.attachScale (D3.bandToScale xAxis) AxisBottom
      >>= D3.selectAllTextFromSelection
      >>= D3.setAttributes
          [ D3.transform $ D3.c [ D3.Rotate (-45.0) 0.0 0.0 ]
          ]
      >>= D3.setStyles
          [ D3.textAnchor $ D3.c "end"
          ]
  yAxis <- D3.scaleLinearCreate { domain: [ 0.0, maxTick ], range: [ svgHeight, 0.0 ] }
  let
    scaleY = D3.scale yAxis
  _ <- D3.appendElement Group svg >>= D3.attachScale (D3.linearToScale yAxis) AxisLeft
  _ <-
    D3.appendElement Group svg
      >>= D3.selectAllElementFromSelection Group
      >>= D3.bindData stackBarChartData
      >>= D3.enter
      >>= D3.appendElement Group
      >>= ( D3.setAttribute
            $ D3.fill
            $ D3.d
                ( D3._stackResultKey
                    >>> \key -> case unit of
                        _
                          | "red" == key -> "red"
                          | "green" == key -> "green"
                          | "blue" == key -> "blue"
                          | otherwise -> "black"
                )
        )
      >>= D3.selectAllElementFromSelection Rect
      >>= D3._bindDataLoopStack
      >>= D3.enter
      >>= D3.appendElement Rect
      >>= D3.setAttributes
          [ D3.x $ D3.d $ D3.scale xAxis <<< _.data.label
          , D3.y $ D3.d $ scaleY <<< D3._stackData1
          , D3.height $ D3.d \d -> scaleY (D3._stackData0 d) - scaleY (D3._stackData1 d)
          , D3.width $ D3.c $ D3.bandwidth xAxis
          ]
  pure unit

verticalGroupedBarChart :: String -> Effect Unit
verticalGroupedBarChart selector = do
  let
    svgHeight = 300.0

    svgWidth = 300.0

    margins = { top: 20.0, right: 20.0, left: 50.0, bottom: 50.0 }

    groups = Array.sort $ Array.nub $ map _.label barChartData

    subgroups = [ "red", "blue", "green" ]
  let
    grouped = Array.groupBy (eq `on` _.label) barChartData

    maxTick = 5.0
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
  xAxis <- scaleBand groups [ 0.0, svgWidth ]
  xSubgroupAxis <-
    D3.scaleBandCreate
      (D3.defaultScaleBandInput { domain: subgroups, range: [ 0.0, D3.bandwidth xAxis ] })
        { padding = Just 0.3
        }
  _ <-
    D3.appendElement Group svg
      >>= D3.setAttribute
          (D3.transform $ D3.c [ D3.Translate 0.0 svgHeight ])
      >>= D3.attachScale (D3.bandToScale xAxis) AxisBottom
      >>= D3.selectAllTextFromSelection
      >>= D3.setAttributes
          [ D3.transform $ D3.c [ D3.Rotate (-45.0) 0.0 0.0 ]
          ]
      >>= D3.setStyles
          [ D3.textAnchor $ D3.c "end"
          ]
  yAxis <- D3.scaleLinearCreate { domain: [ 0.0, maxTick ], range: [ svgHeight, 0.0 ] }
  _ <- D3.appendElement Group svg >>= D3.attachScale (D3.linearToScale yAxis) AxisLeft
  _ <-
    D3._selectAllFromSelection "mytestbar" svg
      >>= D3.bindData grouped
      >>= D3.enter
      >>= D3.appendElement Group
      >>= D3.setAttribute
          ( D3.transform
              $ D3.d
                  ( (\translateX -> [ D3.Translate translateX 0.0 ])
                      <<< D3.scale xAxis
                      <<< _.label
                      <<< NonEmptyArray.head
                  )
          )
      >>= D3.selectAllElementFromSelection Rect
      >>= D3._mapSelectionLoop identity
      >>= D3.enter
      >>= D3.appendElement Rect
      >>= D3.setAttributes
          [ D3.x $ D3.d (D3.scale xSubgroupAxis <<< show <<< _.color)
          , D3.y $ D3.d (\d -> D3.scale yAxis d.value)
          , D3.height $ D3.d (\d -> svgHeight - D3.scale yAxis d.value)
          , D3.width $ D3.c $ D3.bandwidth xSubgroupAxis
          , D3.fill $ D3.d (show <<< _.color)
          ]
  pure unit

-- Similar example, but stacking ourselves rather than using d3's implementation
stackedPercentageBarChart :: String -> Effect Unit
stackedPercentageBarChart selector = do
  let
    svgHeight = 300.0

    svgWidth = 300.0

    margins = { top: 20.0, right: 20.0, left: 50.0, bottom: 50.0 }

    groups = Array.sort $ Array.nub $ map _.label barChartData

    maxTick = 100.0
  let
    grouped =
      map
        ( \group ->
            let
              { head, tail } = NonEmptyArray.uncons group

              total = Foldable.sum $ map _.value group

              toItem lowerBoundPercentage item =
                { label: head.label
                , color: item.color
                , value: item.value
                , percent: item.value / total
                , lowerBoundPercentage
                }
            in
              Foldable.foldr
                ( \subgroup acc ->
                    NonEmptyArray.snoc acc $ toItem (Foldable.sum $ map _.percent acc) subgroup
                )
                (NonEmptyArray.singleton $ toItem 0.0 head)
                tail
        )
        $ Array.groupBy (eq `on` _.label) barChartData
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
  xAxis <- scaleBand groups [ 0.0, svgWidth ]
  _ <-
    D3.appendElement Group svg
      >>= D3.setAttribute (D3.transform $ D3.c [ D3.Translate 0.0 svgHeight ])
      >>= D3.attachScale (D3.bandToScale xAxis) AxisBottom
      >>= D3.selectAllTextFromSelection
      >>= D3.setAttributes
          [ D3.transform $ D3.c [ D3.Rotate (-45.0) 0.0 0.0 ]
          ]
      >>= D3.setStyles
          [ D3.textAnchor $ D3.c "end"
          ]
  yScale <-
    D3.createScaleLinear
      (D3.defaultScaleLinearInput { domain: [ 0.0, maxTick ], range: [ svgHeight, 0.0 ] })
  yAxis <-
    D3.createAxis yScale AxisLeft
      ( D3.emptyAxisConfigNumber
          { ticks = Just 4
          , tickFormat = Just \d _ -> show d <> "%"
          }
      )
  _ <-
    D3.appendElement Group svg
      >>= D3.attachAxis yAxis
  _ <-
    -- Enter on the grouped structure, and then hone in on the inner loop to be able to 
    -- add a rect for each unique group/subgroup relation.
    -- Could also bind a spread out version of grouped (non-nested array) but this seems simpler
    D3._selectAllFromSelection ".event-test-bar" svg
      >>= D3.bindData grouped
      >>= D3.enter
      >>= D3.selectAllElementFromSelection Rect
      >>= D3._mapSelectionLoop identity
      >>= D3.enter
      >>= D3.appendElement Rect
      >>= D3.setAttributes
          [ D3.x $ D3.d $ D3.scale xAxis <<< _.label
          , D3.y $ D3.d
              $ \d ->
                  svgHeight - D3.scale yScale (maxTick * d.lowerBoundPercentage)
          , D3.height $ D3.d \d -> svgHeight - D3.scale yScale (maxTick * d.percent)
          , D3.width $ D3.c $ D3.bandwidth xAxis
          , D3.fill $ D3.di \d _ -> show d.color
          ]
      >>= D3.setEvents
          [ D3.mouseenter
              $ D3.dMe
                  ( \_ d ->
                      void
                        $ D3.selectAllElementFromSelection D3.Rect svg
                        >>= D3.filter (notEq d.color <<< _.color)
                        >>= D3.setStyles
                            [ D3.opacity $ D3.c 0.5
                            ]
                  )
          , D3.mouseleave $ D3.cMe
              $ const
              $ void
              $ D3.selectAllElementFromSelection D3.Rect svg
              >>= D3.setStyles
                  [ D3.opacity $ D3.c 1.0
                  ]
          ]
  pure unit

scaleBand :: Array String -> Array Number -> Effect D3.ScaleBand
scaleBand domain range =
  D3.scaleBandCreate
    (D3.defaultScaleBandInput { domain, range })
      { padding = Just 0.3
      }
