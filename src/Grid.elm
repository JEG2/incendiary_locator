module Grid ( CellDict
            , Grid
            , init
            , width
            , height
            , cells
            , get
            , count
            , placeBombs
            , open
            , toggleFlag ) where

import Array exposing (Array)
import Dict exposing (Dict)
import Random exposing (Seed)
import Set exposing (Set)

import Cell exposing (Cell)


type alias CellDict =
  Dict (Int, Int) Cell

type alias Grid =
  { width : Int
  , height : Int
  , cells : CellDict
  }


init : Int -> Int -> Grid
init width height =
  let
    generateCells w h =
      Array.initialize (w * h) (\i -> (((i % w), (i // w)), (Cell.empty)))
        |> Array.toList
        |> Dict.fromList
  in
    Grid width height (generateCells width height)


width : Grid -> Int
width grid =
  grid.width


height : Grid -> Int
height grid =
  grid.height


cells : Grid -> CellDict
cells grid =
  grid.cells


get : (Int, Int) -> Grid -> Maybe Cell
get xy grid =
  Dict.get xy grid.cells


count : (Cell -> Bool) -> Grid -> Int
count test grid =
  Dict.foldl
    (\_ cell count -> if test cell then count + 1 else count)
    0
    grid.cells


generateBomb : Grid -> Seed -> CellDict -> (Seed, CellDict)
generateBomb grid seed bombs =
  let
    randomX =
      Random.int 0 (grid.width - 1)
    randomY =
      Random.int 0 (grid.height - 1)
    randomXY =
      Random.pair randomX randomY
    randomXYAndSeed =
      Random.generate randomXY seed
    bomb =
      randomXYAndSeed
        |> fst
        |> flip get grid
        |> Maybe.withDefault Cell.empty
        |> Cell.makeBomb
  in
    (snd randomXYAndSeed, Dict.insert (fst randomXYAndSeed) bomb bombs)


generateBombs : Grid -> Int -> Seed -> CellDict -> (Seed, CellDict)
generateBombs grid count seed bombs =
  if (List.length <| Dict.keys <| bombs) == count then
    (seed, bombs)
  else
    uncurry (generateBombs grid count) (generateBomb grid seed bombs)


neighbors : (Int, Int) -> List (Int, Int)
neighbors (x, y) =
  List.map
    (\(xOffset, yOffset) -> (x + xOffset, y + yOffset))
    [ (-1, -1), ( 0, -1), ( 1, -1)
    , (-1,  0),           ( 1,  0)
    , (-1,  1), ( 0,  1), ( 1,  1)
    ]


countNeighbors : CellDict -> (Int, Int) -> Cell -> Cell
countNeighbors cells xy cell =
  let
    toCell neighborXY =
      Dict.get neighborXY cells
        |> Maybe.withDefault Cell.empty
    neighborCells =
      List.map toCell (neighbors xy)
    countIfBomb c count =
      if Cell.isBomb c then count + 1 else count
    bombCount =
      List.foldl countIfBomb 0 neighborCells
  in
    Cell.addNeighbors cell bombCount


countAllNeighbors : CellDict -> CellDict
countAllNeighbors cells =
  Dict.map (countNeighbors cells) cells


addBombsAndCountNeighbors : Int -> Seed -> Grid -> (Seed, CellDict)
addBombsAndCountNeighbors count seed grid =
  let
    seedAndBombs =
      generateBombs grid count seed (Dict.empty)
  in
    grid.cells
      |> Dict.union (snd seedAndBombs)
      |> countAllNeighbors
      |> (,) (fst seedAndBombs)


placeBombs : Int -> Seed -> Grid -> (Seed, Grid)
placeBombs count seed grid =
  let
    seedAndCells =
      addBombsAndCountNeighbors count seed grid
  in
    (fst seedAndCells, {grid | cells <- snd seedAndCells})


openCells : Set (Int, Int) -> Set (Int, Int) -> Grid -> Set (Int, Int)
openCells xys visited grid =
  let
    cell neighborXY =
      Maybe.withDefault (Cell.empty) (get neighborXY grid)
    toNeighbors neighborXY =
      Cell.neighbors (cell neighborXY)
    isBomb neighborXY =
      Cell.isBomb (cell neighborXY)
    neighboringFlags neighborXY =
      neighbors neighborXY
        |> List.filterMap (flip get grid)
        |> List.foldl
             (\cell count -> if Cell.isFlagged cell then count + 1 else count)
             0
    isReopenable neighborXY =
      (Cell.isOpen (cell neighborXY)) &&
      (toNeighbors neighborXY) > 0 &&
      (toNeighbors neighborXY) == (neighboringFlags neighborXY)
    spread neighborXY =
      if isBomb neighborXY then
        [ ]
      else if isReopenable neighborXY then
        neighbors neighborXY
          |> List.filter
               (\xy ->
                  not (Cell.isFlagged
                         (Maybe.withDefault (Cell.empty) (get xy grid))))
      else if (toNeighbors neighborXY) > 0 then
        [ ]
      else
        neighbors neighborXY
    inGrid (x, y) grid =
      x >= 0 && x < grid.width && y >= 0 && y < grid.height
    allVisited =
      Set.union xys visited
    nextXYs =
      Set.toList xys
        |> List.concatMap spread
        |> List.filter (flip inGrid grid)
        |> List.filter (\xy -> not (Set.member xy allVisited))
        |> Set.fromList
  in
    if Set.isEmpty nextXYs then
      allVisited
    else
      openCells nextXYs allVisited grid


open : (Int, Int) -> Grid -> Grid
open xy grid =
  let
    xysToOpen =
      openCells (Set.singleton xy) (Set.empty) grid
    openCell cellXY cell =
      if Set.member cellXY xysToOpen then
        Cell.open cell
      else
        cell
  in
    {grid | cells <- Dict.map openCell grid.cells}


toggleFlag : (Int, Int) -> Grid -> Grid
toggleFlag xy grid =
  let
    flagCell cellXY cell =
      if xy == cellXY then
        Cell.toggleFlag cell
      else
        cell
  in
    {grid | cells <- Dict.map flagCell grid.cells}
