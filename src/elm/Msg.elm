module Msg exposing (..)

import Material


-- LOCAL IMPORTS

import Model exposing (..)
import Keyboard.Extra as Keyboard


type Msg
    = Move Cell
    | Click Int Int
    | PlayMode Mode
    | DisplayWindowSize Int
    | Mdl (Material.Msg Msg)
    | NewMaze
    | GoToNextMaze
    | GoToPreviousMaze
    | GoToMaze Int
    | SetTitle String
    | KeyboardExtraMsg Keyboard.Msg
