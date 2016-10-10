module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Array


-- Project specific imports.

import Material
import TestData exposing (blockSize, gameWindowSize, displayWindowSize)
import Model exposing (Mode(..))


all : Test
all =
    describe "Model"
        [ test "updateCurrentMaze"
            <| \() ->
                let
                    mazes =
                        Just (Model.defaultMaze 10 10 10)
                            |> Model.updateCurrentMaze Model.emptyMazes

                    newMazes =
                        Just (Model.defaultMaze 20 20 20)
                            |> Model.updateCurrentMaze mazes

                    blockSize =
                        case Model.getCurrentMaze newMazes of
                            Just m ->
                                .blockSize m

                            Nothing ->
                                0
                in
                    Expect.equal 20 blockSize
        , test "createNewMaze"
            <| \() ->
                Model.createNewMaze Model.emptyMazes
                    |> Model.createNewMaze
                    |> Model.createNewMaze
                    |> Model.createNewMaze
                    |> Model.createNewMaze
                    |> Model.mazesAsList
                    |> fst
                    |> List.length
                    |> Expect.equal 5
        , test "gotoRecMaze 0"
            <| \() ->
                Just (Model.defaultMaze 1 1 1)
                    |> Model.updateCurrentMaze Model.emptyMazes
                    |> Model.createNewMaze
                    |> Model.createNewMaze
                    |> (flip Model.gotoRecMaze) 0
                    |> Model.getCurrentMaze
                    |> Maybe.withDefault (Model.defaultMaze 10 10 10)
                    |> .gameWindowSize
                    |> Expect.equal 1
        , test "gotoRecMaze 0 with one record"
            <| \() ->
                Just (Model.defaultMaze 1 1 1)
                    |> Model.updateCurrentMaze Model.emptyMazes
                    |> (flip Model.gotoRecMaze) 0
                    |> Model.getCurrentMaze
                    |> Maybe.withDefault (Model.defaultMaze 10 10 10)
                    |> .gameWindowSize
                    |> Expect.equal 1
        , test "gotoRecMaze 1"
            <| \() ->
                Just (Model.defaultMaze 1 1 1)
                    |> Model.updateCurrentMaze Model.emptyMazes
                    |> Model.createNewMaze
                    |> Model.createNewMaze
                    |> (flip Model.gotoRecMaze) 1
                    |> Model.getCurrentMaze
                    |> Maybe.withDefault (Model.defaultMaze 10 10 10)
                    |> .gameWindowSize
                    |> Expect.equal 40
        , test "gotoRecMaze -1"
            <| \() ->
                Just (Model.defaultMaze 1 1 1)
                    |> Model.updateCurrentMaze Model.emptyMazes
                    |> Model.createNewMaze
                    |> Model.createNewMaze
                    |> (flip Model.gotoRecMaze) -1
                    |> Model.getCurrentMaze
                    |> Maybe.withDefault (Model.defaultMaze 10 10 10)
                    |> .gameWindowSize
                    |> Expect.equal 1
        , test "gotoRecMaze 100"
            <| \() ->
                Just (Model.defaultMaze 1 1 1)
                    |> Model.updateCurrentMaze Model.emptyMazes
                    |> Model.createNewMaze
                    |> Model.createNewMaze
                    |> (flip Model.gotoRecMaze) 100
                    |> Model.getCurrentMaze
                    |> Maybe.withDefault (Model.defaultMaze 10 10 10)
                    |> .gameWindowSize
                    |> Expect.equal 40
        , test "gotoNextMaze"
            <| \() ->
                Just (Model.defaultMaze 1 1 1)
                    |> Model.updateCurrentMaze Model.emptyMazes
                    |> Model.createNewMaze
                    |> (flip Model.gotoRecMaze) 0
                    |> Model.gotoNextMaze
                    |> Model.getCurrentMaze
                    |> Maybe.withDefault (Model.defaultMaze 10 10 10)
                    |> .gameWindowSize
                    |> Expect.equal 40
        , test "gotoPreviousMaze"
            <| \() ->
                Just (Model.defaultMaze 1 1 1)
                    |> Model.updateCurrentMaze Model.emptyMazes
                    |> Model.createNewMaze
                    |> Model.gotoPreviousMaze
                    |> Model.getCurrentMaze
                    |> Maybe.withDefault (Model.defaultMaze 10 10 10)
                    |> .gameWindowSize
                    |> Expect.equal 1
        ]
