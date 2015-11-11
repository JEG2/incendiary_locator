module Tests where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import Dict
import Random exposing (Seed)

import Cell exposing (Cell)
import Grid exposing (Grid)


seed : Seed
seed =
  Random.initialSeed 42


-- 1 2 2 1 0
-- 1 X X 1 0
seededGrid : Grid
seededGrid =
  Grid.init 5 2
    |> Grid.placeBombs 2 seed
    |> snd


cell : Test
cell =
  suite "Cell"
    [ test "Empty cells do not have bombs"
           (assert (not <| Cell.isBomb <| Cell.empty))
    , test "Empty cells don't know their neighbor counts"
           (assertEqual 0 (Cell.neighbors <| Cell.empty))
    , test "Empty cells aren't open"
           (assert (not <| Cell.isOpen <| Cell.empty))
    , test "Empty cells aren't flagged"
           (assert (not <| Cell.isFlagged <| Cell.empty))
    , test "Can be made into a bomb"
           (assert (Cell.isBomb <| Cell.makeBomb <| Cell.empty))
    , test "Can have a neighbor count added"
           (assertEqual 7 (Cell.empty
                             |> flip Cell.addNeighbors 7
                             |> Cell.neighbors))
    , test "Can be flagged"
           (assert (Cell.isFlagged <| Cell.toggleFlag <| Cell.empty))
    , test "Can be unflagged"
           (assert
              (not <| Cell.isFlagged <|
                 Cell.toggleFlag <| Cell.toggleFlag <| Cell.empty))
    ]


grid : Test
grid =
  suite "Grid"
    [ test "Grids can be constructed with a desired width"
           (assertEqual 1 (Grid.width <| Grid.init 1 2))
    , test "Grids can be constructed with a desired height"
           (assertEqual 2 (Grid.height <| Grid.init 1 2))
    , test "Grids default all cells to empty on creation"
           (assertEqual (Cell.empty)
                        (Grid.init 1 1
                           |> Grid.get (0, 0)
                           |> Maybe.withDefault (Cell True 1 True True)))
    , test "Grids can add a set number of bombs to random cells"
           (assertEqual 2 (seededGrid
                             |> Grid.count Cell.isBomb))
    , test "Grid bomb placement adds neighbor counts"
           (assert (seededGrid
                      |> Grid.cells
                      |> Dict.values
                      |> List.any (\cell -> cell.neighbors > 0)))
    , test "Opening a bomb only affects that cell"
           (assertEqual 1 (seededGrid
                             |> Grid.open (1, 1)
                             |> Grid.count Cell.isOpen))
    , test "Opening a neighbors count only affects that cell"
           (assertEqual 1 (seededGrid
                             |> Grid.open (0, 1)
                             |> Grid.count Cell.isOpen))
    , test "Opening a bare ground opens all touching bare or numbered cells"
           (assertEqual 4 (seededGrid
                             |> Grid.open (4, 1)
                             |> Grid.count Cell.isOpen))
    , test "Opening a bomb doesn't spread"
           -- 1
           -- X
           (assertEqual 1 (Grid.init 1 2
                             |> Grid.placeBombs 1 seed
                             |> snd
                             |> Grid.open (0, 1)
                             |> Grid.count Cell.isOpen))
    , test "Opening a number with a proper count of flags spreads"
           (assertEqual 5 (seededGrid
                             |> Grid.open (4, 0)
                             |> Grid.toggleFlag (2, 1)
                             |> Grid.open (3, 0)
                             |> Grid.count Cell.isOpen))
    , test "Flagging a cell only affects that cell"
           (assertEqual 1 (seededGrid
                             |> Grid.toggleFlag (1, 1)
                             |> Grid.count Cell.isFlagged))
    ]


all : Test
all =
  suite "Incendiary Locator data structures"
    [ cell
    , grid
    ]
