module MazeGenerate
    exposing
        ( Msg(..)
        , MazeStatus(..)
        , Model
        , emptyModel
        , update
        , Cell
        )

import Matrix exposing (Matrix)
import Process
import Random
import Task


-- LOCAL IMPORTS


type Direction
    = North
    | East
    | South
    | West



{- :
   To Do:
   6. Add tests.
   7. Go through all code and standardize on north or top, etc.
   10. Do the Sidewinder algorithm or one of the other ones.
   11. Turn the whole thing into a package.
-}


type alias Model =
    { cells : Matrix Cell
    , mazeSize : Int
    , currCol : Int
    , currRow : Int
    , status : MazeStatus
    , mazeId : Maybe Int
    , percComplete : Int
    }


type MazeStatus
    = Empty
    | InProcess
    | Complete


emptyModel : Model
emptyModel =
    { cells = Matrix.empty
    , mazeSize = 10
    , currCol = 0
    , currRow = 0
    , status = Empty
    , mazeId = Nothing
    , percComplete = 0
    }


type alias Cell =
    { northLink : Bool
    , eastLink : Bool
    , southLink : Bool
    , westLink : Bool
    }


type Msg
    = BinaryTreeInit Int Int
    | BinaryTreeUpdate Bool
    | BinaryTreeDoRandom
    | BinaryTreeComplete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BinaryTreeInit size mazeId ->
            let
                -- Initialize the cells to the proper size but only if we are not
                -- already processing another maze and have not processed this
                -- one yet.
                ( initializedModel, newCmd ) =
                    if model.status == Empty || model.status == Complete then
                        ( initModel
                            { emptyModel
                                | mazeSize = size
                                , status = InProcess
                                , mazeId = Just mazeId
                                , percComplete = 0
                            }
                        , Random.generate BinaryTreeUpdate Random.bool
                        )
                    else
                        ( model, Cmd.none )
            in
                initializedModel ! [ newCmd ]

        BinaryTreeUpdate bool ->
            let
                ( newModel, newCmd ) =
                    generateBinaryTree model bool

                percComplete =
                    newModel.mazeSize
                        * newModel.mazeSize
                        |> toFloat
                        |> (/) ((toFloat (newModel.currRow * newModel.mazeSize)) + (toFloat newModel.currCol))
                        |> (*) 100.0
                        |> round
            in
                { newModel | percComplete = percComplete } ! [ newCmd ]

        BinaryTreeDoRandom ->
            {- : Generate a random Bool for the BinaryTree algorithm. It is possible
               to directly generate the Bool using BinaryTreeUpdate as the target
               but it will lock up the UI.
            -}
            ( model, Random.generate BinaryTreeUpdate Random.bool )

        BinaryTreeComplete ->
            model ! []


{-| Initialize the cells field of the model with unlinked cells according to
    the mazeSize field.
-}
initModel : Model -> Model
initModel model =
    let
        matrix =
            Matrix.repeat model.mazeSize model.mazeSize (Cell False False False False)

        _ =
            Debug.log "initModel" ("Height: " ++ (toString (Matrix.height matrix)) ++ ", Width: " ++ (toString (Matrix.width matrix)))
    in
        { model | cells = matrix }


{-| Generate a single step of the maze using the Binary Tree algorithm. Returns the
    updated model as well as a Cmd that leads to the next step.
-}
generateBinaryTree : Model -> Bool -> ( Model, Cmd Msg )
generateBinaryTree model bool =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        cell =
            Matrix.get col row model.cells

        ( model1, isCompleted ) =
            case cell of
                Just c ->
                    if col == (model.mazeSize - 1) then
                        link col row c South model
                            |> advanceByCell
                    else if row == (model.mazeSize - 1) then
                        link col row c East model
                            |> advanceByCell
                    else
                        link col
                            row
                            c
                            (if bool then
                                East
                             else
                                South
                            )
                            model
                            |> advanceByCell

                Nothing ->
                    -- If we get here, something is wrong.
                    ( model, True )

        newModel =
            if isCompleted then
                { model1 | status = Complete }
            else
                model1

        newCmd =
            if isCompleted then
                Task.perform (always BinaryTreeComplete) (always BinaryTreeComplete) (Task.succeed True)
            else
                {- : Do this to skip the sleep altogether. This is about three
                   times faster but it ties up the UI.
                -}
                --Random.generate BinaryTreeUpdate Random.bool
                {- : Here we sleep to avoid locking up the screen. The BinaryTreeDoRandom case in
                   update will perform the actual random Bool generation.
                -}
                Task.perform (always BinaryTreeDoRandom) (always BinaryTreeDoRandom) (Process.sleep 0)
    in
        newModel ! [ newCmd ]


{-| Advance the currCol and currRow fields of the model by cell moving by
    row and then cell within row ascending. Return True in the second element
    of the tuple to signify completion.
-}
advanceByCell : Model -> ( Model, Bool )
advanceByCell model =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        ( newModel, isCompleted ) =
            if row /= model.mazeSize then
                if col == (model.mazeSize - 1) then
                    ( { model | currCol = 1, currRow = row + 1 }, False )
                else
                    ( { model | currCol = col + 1 }, False )
            else if col /= (model.mazeSize - 1) then
                ( { model | currCol = col + 1 }, False )
            else
                ( model, True )
    in
        ( newModel, isCompleted )


{-| "Link" two cells by setting the link field on their facing walls to True.
    Linked cells do not have a wall between them.
-}
link : Int -> Int -> Cell -> Direction -> Model -> Model
link col row cell direction model =
    let
        -- Set the directional link in the cell passed and the corresponding cell
        -- if there is one (cells on a border may not have one).
        ( updatedCell, linkedCell, linkedCol, linkedRow ) =
            case direction of
                North ->
                    let
                        linkedCol =
                            col

                        linkedRow =
                            max 0 (row - 1)
                    in
                        ( { cell | northLink = True }
                        , case Matrix.get linkedCol linkedRow model.cells of
                            Nothing ->
                                Nothing

                            Just c ->
                                Just { c | southLink = True }
                        , linkedCol
                        , linkedRow
                        )

                East ->
                    let
                        linkedCol =
                            min model.mazeSize (col + 1)

                        linkedRow =
                            row
                    in
                        ( { cell | eastLink = True }
                        , case Matrix.get linkedCol linkedRow model.cells of
                            Nothing ->
                                Nothing

                            Just c ->
                                Just { c | westLink = True }
                        , linkedCol
                        , linkedRow
                        )

                South ->
                    let
                        linkedCol =
                            col

                        linkedRow =
                            min model.mazeSize (row + 1)
                    in
                        ( { cell | southLink = True }
                        , case Matrix.get linkedCol linkedRow model.cells of
                            Nothing ->
                                Nothing

                            Just c ->
                                Just { c | northLink = True }
                        , linkedCol
                        , linkedRow
                        )

                West ->
                    let
                        linkedCol =
                            min 0 (col - 1)

                        linkedRow =
                            row
                    in
                        ( { cell | westLink = True }
                        , case Matrix.get linkedCol linkedRow model.cells of
                            Nothing ->
                                Nothing

                            Just c ->
                                Just { c | eastLink = True }
                        , linkedCol
                        , linkedRow
                        )

        -- Create a new set of cells with the updated cell and possibly the linked cell.
        newCells =
            case linkedCell of
                Just c2 ->
                    Matrix.update col
                        row
                        (\c ->
                            { c
                                | northLink = updatedCell.eastLink
                                , eastLink = updatedCell.eastLink
                                , southLink = updatedCell.southLink
                                , westLink = updatedCell.southLink
                            }
                        )
                        model.cells
                        |> Matrix.update linkedCol
                            linkedRow
                            (\c ->
                                { c
                                    | northLink = c2.northLink
                                    , eastLink = c2.eastLink
                                    , southLink = c2.southLink
                                    , westLink = c2.southLink
                                }
                            )

                Nothing ->
                    Matrix.update col
                        row
                        (\c ->
                            { c
                                | northLink = updatedCell.northLink
                                , eastLink = updatedCell.eastLink
                                , southLink = updatedCell.southLink
                                , westLink = updatedCell.southLink
                            }
                        )
                        model.cells
    in
        { model | cells = newCells }
