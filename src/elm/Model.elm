module Model exposing (..)

import Dict


{- :
   The gameWindowSize and displayWindowSize are in blockSize units. blockSize is
   in pixels.
-}


type alias Model =
    { cells : Dict.Dict (List Int) Cell
    , blockSize : Int
    , gameWindowSize : Int
    , displayWindowSize : Int
    , center : Cell
    , exit : Maybe Cell
    , isFinished : Bool
    , allowToggleCells : Bool
    }


type alias Cell =
    { x : Int
    , y : Int
    , isWall : Bool
    }
