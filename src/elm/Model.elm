module Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , Difficulty(..)
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
    , mazeDifficulty : Difficulty
    , mazeSizePending : Int
    , mazeGenerate : MG.Model
    , mdl : Material.Model
    , keyboardModel : Keyboard.Model
    , nextId : Int
    }


type alias Maze =
    { cells : Matrix MG.Cell
    , mazeSize : Int
    , center : ( Int, Int )
    , title : String
    , id : Int
    , percComplete : Int
    }


type Mode
    = Editing
    | Playing
    | Viewing


type Difficulty
    = Easy
    | Medium
    | Hard


createMaze : Int -> Int -> Maze
createMaze gwSize id =
    { cells = initialCells gwSize
    , mazeSize = gwSize
    , center = ( 10, 10 )
    , title = "Testing only"
    , id = id
    , percComplete = 0
    }


initialCells : Int -> Matrix MG.Cell
initialCells size =
    Matrix.repeat size size (MG.Cell False False False False)
