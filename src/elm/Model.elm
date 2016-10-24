module Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , createMaze
        )

import Dict
import List.Zipper as Zipper exposing (Zipper)
import Material
import Keyboard.Extra as Keyboard


-- LOCAL IMPORTS

import MazeGenerate as MG


{-| Mazes are stored in a zip list.
-}
type alias Model =
    { mazes : Zipper Maze
    , mazeMode : Mode
    , mazeGenerate : MG.Model
    , mdl : Material.Model
    , keyboardModel : Keyboard.Model
    }


type alias Maze =
    { cells : Dict.Dict (List Int) MG.Cell
    , mazeSize : Int
    , viewportSize : Int
    , center : ( Int, Int )
    , title : String
    }



{- :
      type alias Cell =
          { x : Int
          , y : Int
          , isWall : Bool
          }


   type alias Cell =
       { col : Int
       , row : Int
       , northLink : Bool
       , eastLink : Bool
       , southLink : Bool
       , westLink : Bool
       }
-}


type Mode
    = Editing
    | Playing
    | Viewing


createMaze : Int -> Int -> Maze
createMaze gwSize dwSize =
    { cells = initialCells gwSize
    , mazeSize = gwSize
    , viewportSize = dwSize
    , center = ( 10, 10 )
    , title = "Testing only"
    }


initialCells : Int -> Dict.Dict (List Int) MG.Cell
initialCells windowSize =
    let
        -- top, left, right, bottom as a List
        outerWall =
            List.map (\x -> ( [ x, 1 ], MG.Cell x 1 False False False False )) [1..windowSize]
                ++ List.map (\x -> ( [ 1, x ], MG.Cell 1 x False False False False )) [1..windowSize]
                ++ List.map (\x -> ( [ windowSize, x ], MG.Cell windowSize x False False False False )) [1..windowSize]
                ++ List.map (\x -> ( [ x, windowSize ], MG.Cell x windowSize False False False False )) [1..windowSize]

        -- All cells not on the edge.
        inner =
            List.map (\x -> List.map (\y -> ( [ x, y ], MG.Cell x y False False False False )) [2..(windowSize - 1)]) [2..(windowSize - 1)]
                |> List.concat
    in
        Dict.fromList (outerWall ++ inner)



{- :
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
-}
