module Msg exposing (..)

import Model exposing (..)


type Msg
    = Move Cell
    | Click Int Int
    | ToggleAllowToggle
    | DisplayWindowSize Int
