module Model
    exposing
        ( Model
        , Mazes
        , Maze
        , Cell
        , Mode(..)
        , createMaze
        )

import Array
import Dict
import List.Zipper as Zipper exposing (Zipper)
import Material
import Keyboard.Extra as Keyboard


{- :
   The gameWindowSize and displayWindowSize are in blockSize units. blockSize is
   in pixels.
-}


{-| Mazes are stored in a zip list.
-}
type alias Model =
    { mazes : Zipper Maze
    , mazeMode : Mode
    , mdl : Material.Model
    , keyboardModel : Keyboard.Model
    }


{-| Single constructor union type in order to not expose the internal workings
    of the mazes to the outside world.
-}
type Mazes
    = MazesConstructor
        { preMazes : List Maze
        , currentMaze : Maybe Maze
        , postMazes : List Maze
        }


type alias Maze =
    { cells : Dict.Dict (List Int) Cell
    , blockSize : Int
    , gameWindowSize : Int
    , displayWindowSize : Int
    , center : ( Int, Int )
    , title : String
    }


type alias Cell =
    { x : Int
    , y : Int
    , isWall : Bool
    }


type Mode
    = Editing
    | Playing
    | Viewing


createMaze : Int -> Int -> Int -> Maze
createMaze bSize gwSize dwSize =
    { cells = initialWalls gwSize
    , blockSize = bSize
    , gameWindowSize = gwSize
    , displayWindowSize = dwSize
    , center = ( 10, 10 )
    , title = "Testing only"
    }


initialWalls : Int -> Dict.Dict (List Int) Cell
initialWalls windowSize =
    let
        -- top, left, right, bottom as a List
        outsideWall =
            List.map (\x -> ( [ x, 1 ], Cell x 1 True )) [1..windowSize]
                ++ List.map (\x -> ( [ 1, x ], Cell 1 x True )) [1..windowSize]
                ++ List.map (\x -> ( [ windowSize, x ], Cell windowSize x True )) [1..windowSize]
                ++ List.map (\x -> ( [ x, windowSize ], Cell x windowSize True )) [1..windowSize]

        -- All cells not on the edge.
        inner =
            List.map (\x -> List.map (\y -> ( [ x, y ], Cell x y False )) [2..(windowSize - 1)]) [2..(windowSize - 1)]
                |> List.concat
    in
        Dict.fromList (outsideWall ++ inner)
