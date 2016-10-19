module Model
    exposing
        ( Model
        , Mazes
        , Maze
        , Cell
        , Mode(..)
        , defaultMaze
        , emptyMazes
        , getCurrentMaze
        , updateCurrentMaze
        )

import Dict
import Keyboard.Extra as Keyboard


{- :
   The gameWindowSize and displayWindowSize are in blockSize units. blockSize is
   in pixels.
-}


{-| Mazes are stored in a zip list.
-}
type alias Model =
    { mazes : Mazes
    , mazeMode : Mode
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


emptyMazes : Mazes
emptyMazes =
    MazesConstructor
        { preMazes = []
        , currentMaze = Nothing
        , postMazes = []
        }


defaultMaze : Int -> Int -> Int -> Maze
defaultMaze bSize gwSize dwSize =
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



-- API


getCurrentMaze : Mazes -> Maybe Maze
getCurrentMaze (MazesConstructor { preMazes, currentMaze, postMazes }) =
    currentMaze


updateCurrentMaze : Mazes -> Maybe Maze -> Mazes
updateCurrentMaze (MazesConstructor { preMazes, currentMaze, postMazes }) maze =
    MazesConstructor { preMazes = preMazes, currentMaze = maze, postMazes = postMazes }


gotoPreviousMaze : Mazes -> Mazes
gotoPreviousMaze (MazesConstructor { preMazes, currentMaze, postMazes }) =
    let
        preMazesLen =
            List.length preMazes

        preMazes' =
            if preMazesLen == 0 then
                preMazes
            else
                List.take (preMazesLen - 1) preMazes

        currentMaze' =
            if preMazesLen == 0 then
                currentMaze
            else
                List.drop (preMazesLen - 1) preMazes
                    |> List.head

        postMazes' =
            case currentMaze of
                Just m ->
                    m :: postMazes

                Nothing ->
                    postMazes
    in
        MazesConstructor { preMazes = preMazes', currentMaze = currentMaze', postMazes = postMazes' }


gotoNextMaze : Mazes -> Mazes
gotoNextMaze (MazesConstructor { preMazes, currentMaze, postMazes }) =
    let
        postMazesLen =
            List.length postMazes

        preMazes' =
            case currentMaze of
                Just m ->
                    preMazes ++ [ m ]

                Nothing ->
                    preMazes

        currentMaze' =
            if postMazesLen == 0 then
                currentMaze
            else
                List.head postMazes

        postMazes' =
            if postMazesLen == 0 then
                postMazes
            else
                List.take (postMazesLen - 1) postMazes
    in
        MazesConstructor { preMazes = preMazes', currentMaze = currentMaze', postMazes = postMazes' }
