module IncendiaryLocator where

import Color
import Dict
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Mouse
import Random exposing (Seed)
import Signal exposing (Signal)
import Text
import Time exposing (Time)
import Window

import Cell exposing (Cell)
import Grid exposing (Grid)


-- MODEL

type GameStatus
  = Loading
  | Ready
  | Playing Time
  | Over Bool

type alias Model =
  { status : GameStatus
  , grid : Grid
  , seed : Maybe Seed
  , highlight : Maybe (Int, Int)
  , clicking : Maybe (Int, Int)
  }


init : Model
init =
  Model Loading (Grid.init 8 8) Nothing Nothing Nothing


-- UPDATE

type alias State =
  { time : Time
  , window : (Int, Int)
  , xy : (Int, Int)
  , button : Bool
  , space : Bool
  }


update : State -> Model -> Model
update {time, window, xy, button, space} model =
  case model.status of
    Loading ->
      updateSeed time model
        |> updateStatus time
    Over _ ->
      updateHighlight window xy model
        |> updateClicking button
    _ ->
      updateHighlight window xy model
        |> updateClicking button
        |> updateFlagging space
        |> updateStatus time


updateSeed : Time -> Model -> Model
updateSeed time model =
  let
    seedAndGrid =
      Grid.placeBombs 10 (Random.initialSeed (round time)) model.grid
  in
    case model.seed of
      Just _ ->
        model
      Nothing ->
        { model |
            grid <- snd seedAndGrid
        ,   seed <- Just (fst seedAndGrid)
        }


updateHighlight : (Int, Int) -> (Int, Int) -> Model -> Model
updateHighlight (width, height) (x, y) model =
  let
    borderlessGridSize =
      gridSize - 2
    gridXStart =
      width // 2 - borderlessGridSize // 2
    gridYStart =
      height // 2 - borderlessGridSize // 2 -
      (boardHeight // 2 * -1 + (gridSize // 2 + 20))
    inGrid =
      x >= gridXStart && x < gridXStart + borderlessGridSize &&
      y >= gridYStart && y < gridYStart + borderlessGridSize
    cellX =
      (x - gridXStart) // cellSize
    cellY =
      (y - gridYStart) // cellSize
    gameXStart =
      width // 2 - gameButtonSize // 2
    gameYStart =
      height // 2 - gameButtonSize // 2 -
      (boardHeight // 2 - (gameButtonSize // 2 + 20))
    onGameButton =
      x >= gameXStart && x < gameXStart + gameButtonSize &&
      y >= gameYStart && y < gameYStart + gameButtonSize
  in
    if inGrid then
      {model | highlight <- Just (cellX, cellY)}
    else if onGameButton then
      {model | highlight <- Just (-1, -1)}
    else
      {model | highlight <- Nothing}


updateClicking : Bool -> Model -> Model
updateClicking down model =
  case model.clicking of
    Just xy ->
      if not down then
        if model.clicking == model.highlight then
          if xy == (-1, -1) then
            let
              nextSeed =
                Maybe.withDefault (Random.initialSeed 0) model.seed
              seedAndGrid =
                Grid.init (Grid.width model.grid) (Grid.height model.grid)
                  |> Grid.placeBombs 10 nextSeed
            in
              { model |
                  clicking <- Nothing
              ,   grid <- snd seedAndGrid
              ,   seed <- Just (fst seedAndGrid)
              ,   status <- Ready
              }
          else
            case model.status of
              Over _ ->
                {model | clicking <- Nothing}
              _ ->
               { model |
                   clicking <- Nothing
               ,   grid <- Grid.open xy model.grid
               }
        else
          {model | clicking <- Nothing}
      else
        model
    Nothing ->
      if down then
        {model | clicking <- model.highlight}
      else
        model


updateFlagging : Bool -> Model -> Model
updateFlagging space model =
  case model.highlight of
    Just xy ->
      if space then
        {model | grid <- Grid.toggleFlag xy model.grid}
      else
        model
    Nothing ->
      model


updateStatus : Time -> Model -> Model
updateStatus time model =
  case model.status of
    Loading ->
      case model.seed of
        Just _ ->
          {model | status <- Ready}
        Nothing ->
          model
    Ready ->
      if (Grid.count Cell.isOpen model.grid) > 0 then
        {model | status <- Playing time}
      else
        model
    Playing _ ->
      let
        lost =
          Grid.cells model.grid
            |> Dict.values
            |> List.any (\cell -> (Cell.isOpen cell) && (Cell.isBomb cell))
        won =
          Grid.cells model.grid
            |> Dict.values
            |> List.all
                 (\cell -> (Cell.isOpen cell) ||
                           ((Cell.isBomb cell) && (Cell.isFlagged cell)))
      in
        if lost then
          {model | status <- Over False}
        else if won then
          {model | status <- Over True}
        else
          model
    Over _ ->
      model


-- VIEW

view : (Int, Int) -> Model -> Element
view (width, height) model =
  let
    title =
      Text.fromString "Incendiary Locator"
        |> Text.height 40
        |> Text.color (Color.rgb 255 255 255)
        |> centered
    instructions =
      Text.fromString "* Click cells to open\n"
        |> flip
             Text.append
             (Text.fromString "* Press space over a cell to flag it\n")
        |> flip Text.append (Text.fromString "* Click the face to restart")
        |> Text.height 20
        |> Text.color (Color.rgb 220 220 220)
        |> leftAligned
    content =
      case model.status of
        Loading ->
          Text.fromString "Loading..."
            |> Text.height 40
            |> Text.color (Color.rgb 255 255 255)
            |> centered
        _ ->
          board model
  in
    layers
      [ color (Color.rgb 0 0 0) (container width height middle content)
      , container width height midTop title
      , container width height midBottom instructions
      ]


boardWidth : Int
boardWidth =
  gridSize + 20 * 2


boardHeight : Int
boardHeight =
  gridSize + dashboardHeight + 20 * 3


dashboardWidth : Int
dashboardWidth =
  gridSize


dashboardHeight : Int
dashboardHeight =
  gameButtonSize


gameButtonSize : Int
gameButtonSize =
  64


gridSize : Int
gridSize =
  258


cellSize : Int
cellSize =
  32


board : Model -> Element
board model =
  collage boardWidth boardHeight
    [ chrome
    , dashboard model
    , grid model
    ]


chrome : Form
chrome =
  let
    width =
      toFloat boardWidth
    height =
      toFloat boardHeight
    dashHeight =
      toFloat dashboardHeight
    grid =
      toFloat gridSize
  in
    group
      [ filled
          (Color.rgb 189 189 189)
          (rect (toFloat boardWidth) (toFloat boardHeight))
      , filled
          (Color.rgb 255 255 255)
          (polygon
             [ (width / 2 * -1, height / 2 * -1)
             , (width / 2 * -1 + 3, height / 2 * -1 + 3)
             , (width / 2 * -1 + 3, height / 2 - 3)
             , (width / 2 - 3, height / 2 - 3)
             , (width / 2, height / 2)
             , (width / 2 * -1, height / 2)
             ])
      , filled
          (Color.rgb 122 122 122)
          (polygon
             [ (width / 2 * -1, height / 2 * -1)
             , (width / 2 * -1 + 3, height / 2 * -1 + 3)
             , (width / 2 - 3, height / 2 * -1 + 3)
             , (width / 2 - 3, height / 2 - 3)
             , (width / 2, height / 2)
             , (width / 2, height / 2 * -1)
             ])
      , filled
          (Color.rgb 122 122 122)
          (polygon
             [ (width / 2 * -1 + 17, height / 2 - (20 + dashHeight + 3))
             , (width / 2 * -1 + 20, height / 2 - (20 + dashHeight))
             , (width / 2 * -1 + 20, height / 2 - 20)
             , (width / 2 - 20, height / 2 - 20)
             , (width / 2 - 17, height / 2 - 17)
             , (width / 2 * -1 + 17, height / 2 - 17)
             ])
      , filled
          (Color.rgb 255 255 255)
          (polygon
             [ (width / 2 * -1 + 17, height / 2 - (20 + dashHeight + 3))
             , (width / 2 * -1 + 20, height / 2 - (20 + dashHeight))
             , (width / 2 - 20, height / 2 - (20 + dashHeight))
             , (width / 2 - 20, height / 2 - 20)
             , (width / 2 - 17, height / 2 - 17)
             , (width / 2 - 17, height / 2 - (20 + dashHeight + 3))
             ])
      , filled
          (Color.rgb 122 122 122)
          (polygon
             [ (width / 2 * -1 + 17, height / 2 * -1 + 17)
             , (width / 2 * -1 + 20, height / 2 * -1 + 20)
             , (width / 2 * -1 + 20, height / 2 * -1 + 20 + grid)
             , (width / 2 - 20, height / 2 * -1 + 20 + grid)
             , (width / 2 - 17, height / 2 * -1 + 20 + grid + 3)
             , (width / 2 * -1 + 17, height / 2 * -1 + 20 + grid + 3)
             ])
      , filled
          (Color.rgb 255 255 255)
          (polygon
             [ (width / 2 * -1 + 17, height / 2 * -1 + 17)
             , (width / 2 * -1 + 20, height / 2 * -1 + 20)
             , (width / 2 - 20, height / 2 * -1 + 20)
             , (width / 2 - 20, height / 2 * -1 + 20 + grid)
             , (width / 2 - 17, height / 2 * -1 + 20 + grid + 3)
             , (width / 2 - 17, height / 2 * -1 + 17)
             ])
      ]


dashboard : Model -> Form
dashboard model =
  group [gameButton model]
    |> moveY (toFloat (boardHeight // 2 - (gameButtonSize // 2 + 20)))


gameButton : Model -> Form
gameButton model =
  let
    highlighted =
      case model.highlight of
        Just xy ->
          xy == (-1, -1)
        Nothing ->
          False
    clicked =
      case model.clicking of
        Just xy ->
          xy == (-1, -1)
        Nothing ->
          False
    color =
      if highlighted then Color.rgb 255 255 0 else Color.rgb 189 189 189
    shadow =
      if highlighted then Color.rgb 170 170 0 else Color.rgb 122 122 122
    size =
      (toFloat gameButtonSize)
    cover =
      filled color (square size)
  in
    if clicked then
      group [cover, gameButtonFace model]
    else
      group
        [ cover
        , filled
            (Color.rgb 255 255 255)
            (polygon
              [ (size / 2 * -1, size / 2 * -1)
              , (size / 2 * -1 + 3, size / 2 * -1 + 3)
              , (size / 2 * -1 + 3, size / 2 - 3)
              , (size / 2 - 3, size / 2 - 3)
              , (size / 2, size / 2)
              , (size / 2 * -1, size / 2)
              ])
        , filled
            shadow
            (polygon
              [ (size / 2 * -1, size / 2 * -1)
              , (size / 2 * -1 + 3, size / 2 * -1 + 3)
              , (size / 2 - 3, size / 2 * -1 + 3)
              , (size / 2 - 3, size / 2 - 3)
              , (size / 2, size / 2)
              , (size / 2, size / 2 * -1)
              ])
        , gameButtonFace model
        ]


gameButtonFace : Model -> Form
gameButtonFace model =
  let
    smile =
      collage 16 6
        [ moveY 8 (outlined {defaultLine | width <- 2} (circle 8))
        ]
        |> toForm
    happy =
      group
        [ move (-8, 4) (filled (Color.rgb 0 0 0) (circle 2))
        , move (8, 4) (filled (Color.rgb 0 0 0) (circle 2))
        , moveY -8 smile
        ]
    cool =
      group
        [ move (-8, 4) (filled (Color.rgb 0 0 0) (circle 6))
        , move (8, 4) (filled (Color.rgb 0 0 0) (circle 6))
        , filled
            (Color.rgb 0 0 0)
            (polygon [(-24, 0), (-14, 10), (14, 10), (24, 0), (14, 4), (-14, 4)])
        , moveY -8 smile
        ]
    surprised =
      group
        [ move (-8, 4) (filled (Color.rgb 0 0 0) (circle 4))
        , move (8, 4) (filled (Color.rgb 0 0 0) (circle 4))
        , moveY -8 (outlined {defaultLine | width <- 2} (oval 6 8))
        ]
    sad =
      group
        [ traced {defaultLine | width <- 2} (segment (-10, 6) (-6, 2))
        , traced {defaultLine | width <- 2} (segment (-10, 2) (-6, 6))
        , traced {defaultLine | width <- 2} (segment (6, 6) (10, 2))
        , traced {defaultLine | width <- 2} (segment (6, 2) (10, 6))
        , moveY -8 (rotate (degrees 180) smile)
        ]
    expression =
      case model.status of
        Over won ->
          if won then cool else sad
        _ ->
          case model.clicking of
            Just _ ->
              surprised
            Nothing ->
              happy
  in
    group
      [ filled (Color.rgb 255 255 0) (circle 24)
      , outlined {defaultLine | width <- 2} (circle 24)
      , expression
      ]


grid : Model -> Form
grid model =
  group (gridBackground :: (gridCells model))
    |> moveY (toFloat (boardHeight // 2 * -1 + (gridSize // 2 + 20)))


gridBackground : Form
gridBackground =
  square (toFloat gridSize)
    |> filled (Color.rgb 122 122 122)


gridCells : Model -> List Form
gridCells model =
  List.concatMap
    (\y ->
       List.map
         (\x -> gridCell ((x - 1), (y - 1)) model)
         [1..model.grid.width])
    [1..model.grid.height]


gridCell : (Int, Int) -> Model -> Form
gridCell (x, y) model =
  let
    xOffset =
      toFloat (gridSize // 2 * -1 + x * cellSize + cellSize // 2 + 1)
    yOffset =
      toFloat (negate (gridSize // 2 * -1 + y * cellSize + cellSize // 2 + 1))
    highlight =
      Maybe.withDefault
        (model.grid.width + 1, model.grid.height + 1)
        model.highlight
    highlighted =
      (fst highlight) == x && (snd highlight) == y
    click =
      Maybe.withDefault
        (model.grid.width + 1, model.grid.height + 1)
        model.clicking
    clicked =
      (fst click) == x && (snd click) == y
    cell =
      Maybe.withDefault Cell.empty (Grid.get (x, y) model.grid)
  in
     List.concat
       [ cellBackground highlighted cell
       , cellContents cell
       , cellCover highlighted clicked cell
       ]
       |> group
       |> move (xOffset, yOffset)


cellBackground : Bool -> Cell -> List Form
cellBackground highlighted cell =
  let
    color =
      if (Cell.isBomb cell) && (Cell.isOpen cell) then
        Color.rgb 255 0 0
      else if highlighted && (Cell.neighbors cell) >= 1 then
        Color.rgb 255 255 0
      else
        Color.rgb 189 189 189
  in
    [filled color (square (toFloat (cellSize - 2)))]


cellContents : Cell -> List Form
cellContents cell =
  let
    neighbors =
      Cell.neighbors cell
    color =
      case neighbors of
        1 -> Color.rgb 0 0 255
        2 -> Color.rgb 0 127 0
        3 -> Color.rgb 255 0 0
        4 -> Color.rgb 0 0 127
        5 -> Color.rgb 129 0 0
        6 -> Color.rgb 0 127 127
        7 -> Color.rgb 0 0 0
        _ -> Color.rgb 122 122 122
    toText n =
      toString n
        |> Text.fromString
        |> Text.height 22
        |> Text.color color
        |> text
        |> moveY 3
  in
    if Cell.isBomb cell then
      cellBomb
    else if neighbors == 0 then
      [ ]
    else
      [toText neighbors]


cellBomb : List Form
cellBomb =
  [ filled (Color.rgb 0 0 0) (rect 3 24)
  , move (0, 12) (filled (Color.rgb 0 0 0) (circle 1.5))
  , move (0, -12) (filled (Color.rgb 0 0 0) (circle 1.5))
  , filled (Color.rgb 0 0 0) (rect 24 3)
  , move (-12, 0) (filled (Color.rgb 0 0 0) (circle 1.5))
  , move (12, 0) (filled (Color.rgb 0 0 0) (circle 1.5))
  , filled (Color.rgb 0 0 0) (circle 10)
  , move (-8, 8) (filled (Color.rgb 0 0 0) (circle 2))
  , move (8, 8) (filled (Color.rgb 0 0 0) (circle 2))
  , move (-8, -8) (filled (Color.rgb 0 0 0) (circle 2))
  , move (8, -8) (filled (Color.rgb 0 0 0) (circle 2))
  , move (-3, 3) (filled (Color.rgb 255 255 255) (circle 2))
  ]


cellCover : Bool -> Bool -> Cell -> List Form
cellCover highlighted clicked cell =
  let
    color =
      if highlighted then Color.rgb 255 255 0 else Color.rgb 189 189 189
    shadow =
      if highlighted then Color.rgb 170 170 0 else Color.rgb 122 122 122
    background =
      filled color (square (toFloat cellSize))
    flag =
      if Cell.isFlagged cell then
        [ moveY -10
            (toForm
              (collage 20 4 [moveY -2 (filled (Color.rgb 0 0 0) (oval 20 8))]))
        , filled (Color.rgb 0 0 0) (rect 4 20)
        , filled
            (Color.rgb 255 0 0)
            (polygon [(-2, 2), (2, 2), (2, 10), (-2, 10), (-10, 6)])
        ]
      else
        [ ]
    cover =
      group (background :: flag)
  in
    if Cell.isOpen cell then
      [ ]
    else if clicked then
      [cover]
    else
      [ cover
      , filled
          (Color.rgb 255 255 255)
          (polygon
            [(-16, -16), (-16, 16), (16, 16), (13, 13), (-13, 13), (-13, -13)])
      , filled
          shadow
          (polygon
            [(-16, -16), (16, -16), (16, 16), (13, 13), (13, -13), (-13, -13)])
      ]


-- SIGNALS

seconds : Signal Time
seconds =
  Time.every Time.second


window : Signal (Int, Int)
window =
  Window.dimensions


mousePositions : Signal (Int, Int)
mousePositions =
  Signal.sampleOn (Time.fps 30) (Mouse.position)
    |> Signal.dropRepeats


inputs : Signal State
inputs =
  Signal.map5 State seconds window mousePositions Mouse.isDown Keyboard.space


state : Signal Model
state =
  Signal.foldp update init inputs


main : Signal Element
main =
  Signal.map2 view window state
