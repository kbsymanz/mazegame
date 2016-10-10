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
        , createNewMaze
        , gotoPreviousMaze
        , gotoNextMaze
        , mazesAsList
        , gotoRecMaze
        )

import Array
import Dict
import Material


{- :
   The gameWindowSize and displayWindowSize are in blockSize units. blockSize is
   in pixels.
-}


{-| Mazes are stored in a zip list.
-}
type alias Model =
    { mazes : Mazes
    , mazeMode : Mode
    , mdl : Material.Model
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


createNewMaze : Mazes -> Mazes
createNewMaze (MazesConstructor { preMazes, currentMaze, postMazes }) =
    let
        newMaze =
            defaultMaze 10 40 20

        postMazes' =
            newMaze :: postMazes

        newMazes =
            MazesConstructor { preMazes = preMazes, currentMaze = currentMaze, postMazes = postMazes' }
    in
        gotoNextMaze newMazes


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


{-| Returns a Mazes with the current record set to the zero-based index passed.
-}
gotoRecMaze : Mazes -> Int -> Mazes
gotoRecMaze (MazesConstructor { preMazes, currentMaze, postMazes }) idx =
    let
        preMazesLen =
            List.length preMazes

        postMazesLen =
            List.length postMazes

        ( currMazeLen, currMazeAsList ) =
            case currentMaze of
                Just m ->
                    ( 1, [ m ] )

                Nothing ->
                    ( 0, [] )

        totalMazeLen =
            preMazesLen + currMazeLen + postMazesLen

        mazesList =
            preMazes ++ currMazeAsList ++ postMazes

        mazesArray =
            Array.fromList mazesList

        mazes =
            -- Index is zero or less, go to first record.
            if idx <= 0 then
                MazesConstructor
                    { preMazes = []
                    , currentMaze =
                        if preMazesLen > 0 then
                            List.head preMazes
                        else
                            currentMaze
                    , postMazes =
                        (case List.tail (preMazes ++ currMazeAsList ++ postMazes) of
                            Just a ->
                                a

                            Nothing ->
                                []
                        )
                    }
                -- Index exceeds the number records, go to last record.
            else if idx >= totalMazeLen then
                MazesConstructor
                    { preMazes = List.take (totalMazeLen - 1) mazesList
                    , currentMaze = List.drop (totalMazeLen - 1) mazesList |> List.head
                    , postMazes = []
                    }
            else
                MazesConstructor
                    { preMazes = Array.toList <| Array.slice 0 idx mazesArray
                    , currentMaze = Array.get idx mazesArray
                    , postMazes = Array.toList <| Array.slice (idx + 1) (Array.length mazesArray) mazesArray
                    }
    in
        mazes


mazesAsList : Mazes -> ( List Maze, Int )
mazesAsList (MazesConstructor { preMazes, currentMaze, postMazes }) =
    let
        ( currMaze, currMazeIdx ) =
            case currentMaze of
                Just m ->
                    ( [ m ], List.length preMazes )

                Nothing ->
                    ( [], -1 )

        mazes =
            preMazes ++ currMaze ++ postMazes
    in
        ( mazes, currMazeIdx )
