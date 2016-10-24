module Msg exposing (..)

import Material


-- LOCAL IMPORTS

import MazeGenerate as MG exposing (..)
import Model exposing (..)
import Keyboard.Extra as Keyboard


type Msg
    = Move Cell
    | PlayMode Mode
    | ViewportSize Int
    | Mdl (Material.Msg Msg)
    | NewMaze
    | GoToNextMaze
    | GoToPreviousMaze
    | GoToMaze Int
    | SetTitle String
    | KeyboardExtraMsg Keyboard.Msg
    | MazeGenerate MG.Msg
