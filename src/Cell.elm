module Cell where


type alias Cell =
  { isBomb : Bool
  , neighbors: Int
  , isOpen : Bool
  , isFlagged : Bool
  }


empty : Cell
empty =
  Cell False 0 False False


isBomb : Cell -> Bool
isBomb cell =
  cell.isBomb


neighbors : Cell -> Int
neighbors cell =
  cell.neighbors


isOpen : Cell -> Bool
isOpen cell =
  cell.isOpen


isFlagged : Cell -> Bool
isFlagged cell =
  cell.isFlagged


makeBomb : Cell -> Cell
makeBomb cell =
  {cell | isBomb <- True}


addNeighbors : Cell -> Int -> Cell
addNeighbors cell count =
  {cell | neighbors <- count}


open : Cell -> Cell
open cell =
  {cell | isOpen <- True}


toggleFlag : Cell -> Cell
toggleFlag cell =
  {cell | isFlagged <- not cell.isFlagged}
