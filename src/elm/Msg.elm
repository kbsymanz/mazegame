module Msg exposing (..)

import Model exposing (..)


type Msg
    = Move Cell
    | Click Int Int
    | PlayMode Mode
    | DisplayWindowSize Int
