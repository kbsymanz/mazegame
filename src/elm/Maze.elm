module Maze exposing (..)

import Array
import Dict
import Html.App as App
import List.Zipper as Zipper exposing (Zipper)
import Material
import Keyboard.Extra as Keyboard


-- LOCAL IMPORTS

import MazeGenerate as MG
import Model
    exposing
        ( Model
        , Maze
        , Mode(..)
        , createMaze
        )
import Msg exposing (Msg(..))
import View as V


-- MODEL


{-| The number of blocks the maze is horizontally and
    vertically.
-}
mazeSize : Int
mazeSize =
    20


{-| The number of blocks the user can currently see horizontally
    and vertically.
-}
viewportSize : Int
viewportSize =
    60


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.init
    in
        ( { mazes = Zipper.singleton <| createMaze mazeSize viewportSize
          , mazeMode = Viewing
          , mazeGenerate = MG.emptyModel
          , mdl = Material.model
          , keyboardModel = keyboardModel
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        Mdl mdlMsg ->
            Material.update mdlMsg model

        PlayMode mode ->
            { model | mazeMode = mode } ! []

        ViewportSize size ->
            let
                newMazes =
                    Zipper.update (\m -> { m | viewportSize = size }) model.mazes
            in
                { model | mazes = newMazes } ! []

        NewMaze ->
            -- Inserts the new maze after the current maze and makes it current.
            let
                newMaze =
                    createMaze 40 40

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

                center =
                    currentmaze.center

                ms =
                    currentmaze.mazeSize

                centery =
                        snd center

                centerx =
                        fst center

                newx =
                    case arrows.x of
                        -1 ->
                            if centerx > 1 then
                                centerx - 1
                            else
                                centerx

                        1 ->
                            if centerx == ms then
                                centerx
                            else
                                centerx + 1

                        _ ->
                            centerx

                newy =
                    case arrows.y of
                        -1 ->
                            -- down arrow
                            if centery < ms then
                                centery + 1
                            else
                                centery

                        1 ->
                            -- up arrow
                            if centery == 1 then
                                centery
                            else
                                centery - 1

                        _ ->
                            centery

                updatedMazes =
                    Zipper.update (\m -> { m | center = ( newx, newy ) }) model.mazes

            in
                if model.mazeMode == Playing then
                    ( { model
                        | keyboardModel = keyboardModel
                        , mazes = updatedMazes
                      }
                    , Cmd.map KeyboardExtraMsg keyboardCmd
                    )
                else model ! []

        MazeGenerate mgMsg ->
            let
                ( mgModel, mgCmd ) =
                    MG.update mgMsg model.mazeGenerate

                newMazes =
                    Zipper.current model.mazes
                        |> (\m -> Zipper.update (always { m | cells = mgModel.cells }) model.mazes)
            in
                ( { model | mazeGenerate = mgModel, mazes = newMazes }, Cmd.map (\m -> MazeGenerate m) mgCmd )



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
