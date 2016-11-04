module Msg exposing (..)

import Material


-- LOCAL IMPORTS

import MazeGenerate as MG exposing (..)
import Model exposing (..)
import Keyboard.Extra as Keyboard


type Msg
    = PlayMode Mode
    | MazeDifficulty Difficulty
    | MazeSizePending Int
    | Mdl (Material.Msg Msg)
    | NewMaze
    | DeleteMaze Int
    | GoToNextMaze
    | GoToPreviousMaze
    | GoToMaze Int
    | SetTitle String
    | KeyboardExtraMsg Keyboard.Msg
    | MazeGenerate MG.Msg
