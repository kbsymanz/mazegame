module Maze exposing (..)

import Array
import Dict
import Html.App as App
import List.Zipper as Zipper exposing (Zipper)
import Material
import Keyboard.Extra as Keyboard


-- LOCAL IMPORTS

import View as V
import Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , Cell
        , createMaze
        )
import Msg exposing (Msg(..))


-- MODEL


{-| The number of blocks the maze is horizontally and
    vertically. Blocks are blockSize pixels in size each.
-}
gameWindowSize : Int
gameWindowSize =
    20


{-| The number of blocks the user can currently see horizontally
    and vertically. Blocks are blockSize pixels in size each.
-}
displayWindowSize : Int
displayWindowSize =
    20


{-| The number of pixels that make up the width and height of
    each block.
-}
blockSize : Int
blockSize =
    40


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.init
    in
        ( { mazes = Zipper.singleton <| createMaze blockSize gameWindowSize displayWindowSize
          , mazeMode = Viewing
          , mdl = Material.model
          , keyboardModel = keyboardModel
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        Mdl msg' ->
            Material.update msg' model

        Move cell ->
            let
                newMazes =
                    Zipper.update (\m -> { m | cells = (Dict.insert [ cell.x, cell.y ] cell m.cells) }) model.mazes
            in
                { model | mazes = newMazes } ! []

        Click x y ->
            -- Toggle whether the chosen cell is a wall or not.
            let
                currMaze =
                    Zipper.current model.mazes

                newMaze =
                    case
                        currMaze
                            |> .cells
                            |> Dict.get [ x, y ]
                    of
                        Just c ->
                            { c | isWall = not c.isWall }
                                |> (\c -> { currMaze | cells = (Dict.insert [ x, y ] c currMaze.cells) })

                        Nothing ->
                            currMaze

                newMazes =
                    Zipper.update (always newMaze) model.mazes
            in
                if model.mazeMode == Editing then
                    { model | mazes = newMazes } ! []
                else
                    model ! []

        PlayMode mode ->
            { model | mazeMode = mode } ! []

        DisplayWindowSize size ->
            let
                newMazes =
                    Zipper.update (\m -> { m | displayWindowSize = size }) model.mazes
            in
                { model | mazes = newMazes } ! []

        NewMaze ->
            -- Inserts the new maze after the current maze and makes it current.
            let
                newMaze =
                    createMaze 10 40 20

                newMazes =
                    case
                        Zipper.after model.mazes
                            |> (::) newMaze
                            |> always
                            |> flip Zipper.updateAfter model.mazes
                            |> Zipper.next
                    of
                        Just m ->
                            m

                        Nothing ->
                            -- This should not be possible.
                            model.mazes

                newModel =
                    { model | mazes = newMazes }
            in
                newModel ! []

        GoToPreviousMaze ->
            let
                newMazes =
                    case Zipper.previous model.mazes of
                        Just m ->
                            m

                        Nothing ->
                            model.mazes
            in
                { model | mazes = newMazes } ! []

        GoToNextMaze ->
            let
                newMazes =
                    case Zipper.next model.mazes of
                        Just m ->
                            m

                        Nothing ->
                            model.mazes
            in
                { model | mazes = newMazes } ! []

        GoToMaze idx ->
            let
                mazesList =
                    Zipper.toList model.mazes

                before =
                    List.take idx mazesList

                curr =
                    mazesList
                        |> Array.fromList
                        |> Array.get idx

                after =
                    List.drop (idx + 1) mazesList

                newMazes =
                    case curr of
                        Just c ->
                            Zipper.singleton c
                                |> Zipper.updateBefore (always before)
                                |> Zipper.updateAfter (always after)

                        Nothing ->
                            model.mazes
            in
                { model | mazes = newMazes } ! []

        SetTitle title ->
            let
                newMazes =
                    Zipper.current model.mazes
                        |> (\m -> Zipper.update (always { m | title = title }) model.mazes)
            in
                { model | mazes = newMazes } ! []

        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.update keyMsg model.keyboardModel

                arrows =
                    Keyboard.arrows keyboardModel

                isarrows =
                    .x arrows /= 0 || .y arrows /= 0

                currentmaze =
                        Zipper.current model.mazes

                center = currentmaze.center

                newx =
                    case arrows.x of
                            -1 ->
                                if fst center > 0 then
                                        (fst center) - 1
                                else
                                    fst center
                            1 -> if (fst center) > currentmaze.gameWindowSize then
                                    fst center
                                 else (fst center) + 1
                            _ -> fst center
                newy =
                    case arrows.y of
                            -1 ->
                                if snd center > 0 then
                                    (snd center) - 1
                                else
                                    snd center
                            1 -> if (snd center) > currentmaze.gameWindowSize then
                                    snd center
                                 else (snd center) + 1
                            _ -> snd center
                updatedMazes =
                    Zipper.update (\m -> { m | center = (newx,newy)}) model.mazes
                _ =
                    Debug.log "keyboardExtraMsg, hello, this is cool" isarrows
            in
                ( { model
                    | keyboardModel = keyboardModel
                    , mazes = updatedMazes
                  }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )





-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.subscriptions
        ]


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = V.view
        , subscriptions = subscriptions
        }
