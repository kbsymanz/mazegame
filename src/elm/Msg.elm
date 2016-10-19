module Msg exposing (..)

import Model exposing (..)
import Keyboard.Extra as Keyboard


type Msg
    = Move Cell
    | Click Int Int
    | PlayMode Mode
    | DisplayWindowSize Int
    | KeyboardExtraMsg Keyboard.Msg
