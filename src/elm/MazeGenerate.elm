module MazeGenerate
    exposing
        ( Msg(..)
        , MazeStatus(..)
        , Direction(..)
        , Model
        , Cell
        , emptyModel
        , update
        , initModel
          -- The functions below are exposed for testing purposes only.
        , advanceByCell
        , generateBinaryTreeStep
        , link
        , generateRecursiveBacktracker
        , neighbors
        , discernLinkDir
        , addCellLinks
        )

import Array
import List.Extra as LE
import Matrix exposing (Matrix)
import Process
import Random
import Task
import Time


-- LOCAL IMPORTS


type Direction
    = North
    | East
    | South
    | West


type alias Model =
    { cells : Matrix Cell
    , mazeSize : Int
    , currCol : Int
    , currRow : Int
    , work : List ( Int, Int )
    , done : List ( Int, Int )
    , status : MazeStatus
    , mazeId : Maybe Int
    , percComplete : Int
    , seed : Random.Seed
    }


type alias WorkUnit =
    ( ( Int, Int ), Cell )


type MazeStatus
    = Empty
    | InProcess
    | Complete
    | Stopped


emptyModel : Model
emptyModel =
    { cells = Matrix.empty
    , mazeSize = 10
    , currCol = 0
    , currRow = 0
    , work = []
    , done = []
    , status = Empty
    , mazeId = Nothing
    , percComplete = 0
    , seed = Random.initialSeed 0
    }


type alias Cell =
    { northLink : Bool
    , eastLink : Bool
    , southLink : Bool
    , westLink : Bool
    }


initializeMaze : Int -> Int -> Model -> Model
initializeMaze size mazeId model =
    let
        initializedModel =
            if model.status /= InProcess then
                initModel
                    { emptyModel
                        | mazeSize = size
                        , status = InProcess
                        , mazeId = Just mazeId
                        , percComplete = 0
                    }
            else
                model
    in
        initializedModel


type Msg
    = Init
    | Error String
    | BinaryTreeInit Int Int
    | BinaryTreeUpdate Bool
    | BinaryTreeDoRandom
    | BinaryTreeComplete
    | MazeGenerationStop
    | NewSeed Time.Time
    | RecursiveBacktrackerInit Int Int
    | RecursiveBacktrackerUpdate
    | RecursiveBacktrackerComplete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            emptyModel ! [ Time.now |> Task.perform (always Error "") NewSeed ]

        Error e ->
            model ! []

        NewSeed time ->
            { model | seed = (Random.initialSeed <| round time) } ! []

        BinaryTreeInit size mazeId ->
            let
                initializedModel =
                    initializeMaze size mazeId model

                newCmd =
                    if model.status /= InProcess then
                        Random.generate BinaryTreeUpdate Random.bool
                    else
                        Cmd.none
            in
                initializedModel ! [ newCmd ]

        BinaryTreeUpdate bool ->
            let
                -- Get a number of links done without coming up for air.
                ( batchModel, _ ) =
                    if model.status == Stopped then
                        ( model, Cmd.none )
                    else
                        generateBinaryTreeBatch model

                -- This will do one more link but return a Cmd that
                -- allows TEA to respond to other events.
                ( newModel, newCmd ) =
                    if model.status == Stopped then
                        ( model, Cmd.none )
                    else
                        generateBinaryTreeStep bool batchModel

                percComplete =
                    if model.status /= Stopped then
                        newModel.mazeSize
                            * newModel.mazeSize
                            |> toFloat
                            |> (/) ((toFloat (newModel.currRow * newModel.mazeSize)) + (toFloat newModel.currCol))
                            |> (*) 100.0
                            |> round
                    else
                        0
            in
                { newModel | percComplete = percComplete } ! [ newCmd ]

        BinaryTreeDoRandom ->
            {- : Generate a random Bool for the BinaryTree algorithm. It is possible
               to directly generate the Bool using BinaryTreeUpdate as the target
               but it will lock up the UI.
            -}
            ( model, Random.generate BinaryTreeUpdate Random.bool )

        BinaryTreeComplete ->
            { model | percComplete = 100 } ! []

        MazeGenerationStop ->
            let
                newStatus =
                    if model.status == InProcess then
                        Stopped
                    else
                        model.status
            in
                { model | status = newStatus } ! []

        RecursiveBacktrackerInit size mazeId ->
            let
                initializedModel =
                    initializeMaze size mazeId model

                -- Choose a random cell coordinates.
                ( ( col, row ), seed ) =
                    Random.step (Random.pair (Random.int 0 (size - 1)) (Random.int 0 (size - 1))) model.seed

                -- Initialize the starting point of maze generation with a random cell.
                -- We can create the cell on the fly because we know that it has no links
                -- and it does not hold more information than that.
                adjustedModel =
                    { initializedModel | work = [ ( col, row ) ] }

                newCmd =
                    if model.status /= InProcess then
                        Task.perform (always RecursiveBacktrackerUpdate) (always RecursiveBacktrackerUpdate) (Task.succeed True)
                    else
                        Cmd.none
            in
                { adjustedModel | seed = seed } ! [ newCmd ]

        RecursiveBacktrackerUpdate ->
            let
                ( newModel, isComplete ) =
                    generateRecursiveBacktracker model

                percComplete =
                    if model.status /= Stopped then
                        List.length newModel.done
                            |> toFloat
                            |> flip (/) (toFloat (model.mazeSize * model.mazeSize))
                            |> (*) 100.0
                            |> round
                    else
                        0

                newCmd =
                    (if isComplete then
                        let
                            _ =
                                Debug.log "newCmd" "DONE"
                        in
                            RecursiveBacktrackerComplete
                     else
                        RecursiveBacktrackerUpdate
                    )
                        |> (\m -> Task.perform (always m) (always m) <| Process.sleep 0)
            in
                { newModel | percComplete = percComplete } ! [ newCmd ]

        RecursiveBacktrackerComplete ->
            { model | percComplete = 100, status = Complete } ! []


{-| Initialize the cells field of the model with unlinked cells according to
    the mazeSize field.
-}
initModel : Model -> Model
initModel model =
    let
        matrix =
            Matrix.repeat model.mazeSize model.mazeSize (Cell False False False False)
    in
        { model | cells = matrix }


neighbors : Int -> Int -> Int -> List ( Int, Int )
neighbors col row mazeSize =
    let
        neighbors =
            [ ( col, row - 1 )
            , ( col + 1, row )
            , ( col, row + 1 )
            , ( col - 1, row )
            ]
                |> List.filter (\c -> fst c >= 0 && fst c < mazeSize && snd c >= 0 && snd c < mazeSize)
    in
        neighbors


discernLinkDir : Int -> Int -> Int -> Int -> Maybe Direction
discernLinkDir col1 row1 col2 row2 =
    case col1 == col2 of
        True ->
            case row1 == row2 of
                True ->
                    -- Cell 1 is the same as cell 2.
                    Nothing

                False ->
                    if abs (row1 - row2) > 1 then
                        -- None adjacent rows.
                        Nothing
                    else
                        if row1 < row2 then
                            Just South
                        else
                            Just North

        False ->
            case col1 < col2 of
                True ->
                    if abs (col1 - col2) > 1 then
                        -- None adjacent columns.
                        Nothing
                    else
                        case row1 == row2 of
                            True ->
                                Just East

                            False ->
                                -- The cells are not adjacent with each other.
                                Nothing

                False ->
                    if abs (col1 - col2) > 1 then
                        -- None adjacent columns.
                        Nothing
                    else
                        case row1 == row2 of
                            True ->
                                Just West

                            False ->
                                -- The cells are not adjacent with each other.
                                Nothing


generateRecursiveBacktracker : Model -> ( Model, Bool )
generateRecursiveBacktracker model =
    let
        isComplete =
            List.isEmpty model.work

        usableNeighbors =
            case List.head model.work of
                Just cellCoordinates ->
                    let
                        ( c, r ) =
                            ( fst cellCoordinates, snd cellCoordinates )

                        -- Neighbors minus those already in our work and done fields.
                        usableNeighbors =
                            neighbors c r model.mazeSize
                                |> List.filter (\i -> not (List.member i model.work))
                                |> List.filter (\i -> not (List.member i model.done))
                    in
                        usableNeighbors

                Nothing ->
                    -- isComplete should also be True.
                    []

        ( newWork, newSeed, newDone ) =
            case List.isEmpty usableNeighbors of
                True ->
                    -- No neighbors, so drop current work from the front
                    -- of work and allow next iteration to continue.
                    ( List.drop 1 model.work, model.seed, model.done )

                False ->
                    let
                        ( index, seed ) =
                            Random.step (Random.int 0 <| flip (-) 1 <| List.length usableNeighbors) model.seed
                        ( newWork, newDone ) =
                            case LE.getAt index usableNeighbors of
                                Just (c2, r2) ->
                                    ( (c2, r2) :: model.work, (c2, r2) :: model.done )

                                Nothing ->
                                    -- Should not get here.
                                    let
                                        _ =
                                            Debug.log "Invalid index into usableNeighbors" <| toString index
                                        _ =
                                            Debug.log "usableNeighbors" <| toString usableNeighbors
                                    in
                                        ( model.work, model.done )
                    in
                        ( newWork, seed, newDone )
        newCells =
            let
                newCell1 =
                    LE.getAt 1 newWork

                newCell2 =
                    LE.getAt 0 newWork

                cells =
                    case newCell1 of
                        Just c1 ->
                            case newCell2 of
                                Just c2 ->
                                    let
                                        dir =
                                            discernLinkDir (fst c1) (snd c1) (fst c2) (snd c2)
                                        cell =
                                            Matrix.get (fst c1) (snd c1) model.cells

                                        newModel =
                                            case dir of
                                                Just d ->
                                                    case cell of
                                                        Just c ->
                                                            link (fst c1) (snd c1) c d model
                                                        Nothing ->
                                                            let
                                                                _ =
                                                                    Debug.log "apple" <| toString True
                                                            in
                                                                model

                                                Nothing ->
                                                    let
                                                        _ =
                                                            Debug.log "banana" <| toString True
                                                    in
                                                        model
                                    in
                                        newModel.cells

                                Nothing ->
                                    let
                                        _ =
                                            Debug.log "cat" <| toString True
                                    in
                                        model.cells

                        Nothing ->
                            model.cells

            in
                cells
    in
        ( { model | work = newWork, cells = newCells, seed = newSeed, done = newDone }, isComplete )


addCellLinks : Cell -> Cell -> Maybe Direction -> ( Cell, Cell )
addCellLinks cell1 cell2 dir =
    case dir of
        Just d ->
            case d of
                North ->
                    ( { cell1 | northLink = True }, { cell2 | southLink = True } )

                East ->
                    ( { cell1 | eastLink = True }, { cell2 | westLink = True } )

                South ->
                    ( { cell1 | southLink = True }, { cell2 | northLink = True } )

                West ->
                    ( { cell1 | westLink = True }, { cell2 | eastLink = True } )

        Nothing ->
            ( cell1, cell2 )


{-| Do four links at once without entering the TEA cycle.
This allows for faster maze generation. This needs
to be interspersed with allowances for the TEA cycle in
order to allow the UI to be somewhat responsive during
maze generation.
-}
generateBinaryTreeBatch : Model -> ( Model, Cmd Msg )
generateBinaryTreeBatch model =
    let
        ( boolList, newSeed ) =
            Random.step (Random.list 5 Random.bool) model.seed

        ( b1, b2, b3, b4, b5 ) =
            ( Maybe.withDefault False <| List.head boolList
            , Maybe.withDefault False <| List.head <| List.drop 1 boolList
            , Maybe.withDefault False <| List.head <| List.drop 2 boolList
            , Maybe.withDefault False <| List.head <| List.drop 3 boolList
            , Maybe.withDefault False <| List.head <| List.drop 4 boolList
            )

        ( model1, isComplete1 ) =
            generateBinaryTreeLink b1 model

        ( model2, isComplete2 ) =
            if not isComplete1 then
                generateBinaryTreeLink b2 model1
            else
                ( model1, True )

        ( model3, isComplete3 ) =
            if not isComplete2 then
                generateBinaryTreeLink b3 model2
            else
                ( model2, True )

        ( model4, isComplete4 ) =
            if not isComplete3 then
                generateBinaryTreeLink b4 model3
            else
                ( model3, True )

        ( model5, isComplete5 ) =
            if not isComplete4 then
                generateBinaryTreeLink b5 model4
            else
                ( model4, True )
    in
        { model5 | seed = newSeed } ! []


generateBinaryTreeLink : Bool -> Model -> ( Model, Bool )
generateBinaryTreeLink bool model =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        cell =
            Matrix.get col row model.cells

        ( model1, isCompleted ) =
            case cell of
                Just c ->
                    if row /= (model.mazeSize - 1) then
                        -- Not the last row.
                        if col == (model.mazeSize - 1) then
                            -- Last column of row, only go south.
                            link col row c South model
                                |> advanceByCell
                        else
                            -- Otherwise, randomly choose a direction.
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
                    else if col == (model.mazeSize - 1) then
                        -- Last cell of last row.
                        ( model, True )
                    else
                        -- Last row but not last cell, only go east.
                        link col row c East model
                            |> advanceByCell

                Nothing ->
                    -- If we get here, something is wrong.
                    ( model, True )

        newModel =
            if isCompleted then
                { model1 | status = Complete }
            else
                model1
    in
        ( newModel, isCompleted )


{-| Generate a single step of the maze using the Binary Tree algorithm. Returns the
    updated model as well as a Cmd that leads to the next step.
-}
generateBinaryTreeStep : Bool -> Model -> ( Model, Cmd Msg )
generateBinaryTreeStep bool model =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        cell =
            Matrix.get col row model.cells

        ( model1, isCompleted ) =
            case cell of
                Just c ->
                    if row /= (model.mazeSize - 1) then
                        -- Not the last row.
                        if col == (model.mazeSize - 1) then
                            -- Last column of row, only go south.
                            link col row c South model
                                |> advanceByCell
                        else
                            -- Otherwise, randomly choose a direction.
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
                    else if col == (model.mazeSize - 1) then
                        -- Last cell of last row.
                        ( model, True )
                    else
                        -- Last row but not last cell, only go east.
                        link col row c East model
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
    cell within row then by row ascending. Return True in the second element
    of the tuple to signify completion.
-}
advanceByCell : Model -> ( Model, Bool )
advanceByCell model =
    let
        ( col, row ) =
            ( model.currCol, model.currRow )

        ( newModel, isCompleted ) =
            if row /= (model.mazeSize - 1) then
                if col == (model.mazeSize - 1) then
                    ( { model | currCol = 0, currRow = row + 1 }, False )
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
                        ( linkedCol, linkedRow ) =
                            ( col, row - 1 )
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
                        ( linkedCol, linkedRow ) =
                            ( col + 1, row )
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
                        ( linkedCol, linkedRow ) =
                            ( col, row + 1 )
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
                        ( linkedCol, linkedRow ) =
                            ( col - 1, row )
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
                                | northLink = updatedCell.northLink
                                , eastLink = updatedCell.eastLink
                                , southLink = updatedCell.southLink
                                , westLink = updatedCell.westLink
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
                                    , westLink = c2.westLink
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
                                , westLink = updatedCell.westLink
                            }
                        )
                        model.cells
    in
        { model | cells = newCells }
