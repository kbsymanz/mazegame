module Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , createMaze
        )

import Keyboard.Extra as Keyboard
import List.Zipper as Zipper exposing (Zipper)
import Material
import Matrix exposing (Matrix)


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
    , nextId : Int
    }


type alias Maze =
    { cells : Matrix MG.Cell
    , mazeSize : Int
    , viewportSize : Int
    , center : ( Int, Int )
    , title : String
    , id : Int
    , percComplete : Int
    }


type Mode
    = Editing
    | Playing
    | Viewing


createMaze : Int -> Int -> Int -> Maze
createMaze gwSize dwSize id =
    { cells = initialCells gwSize
    , mazeSize = gwSize
    , viewportSize = dwSize
    , center = ( 10, 10 )
    , title = "Testing only"
    , id = id
    , percComplete = 0
    }


initialCells : Int -> Matrix MG.Cell
initialCells size =
    Matrix.repeat size size (MG.Cell False False False False)
