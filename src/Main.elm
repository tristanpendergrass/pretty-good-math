port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Config exposing (config)
import FeatherIcons
import Game exposing (CompletedGame, Game)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Json.Encode as E
import Math
import Random
import Task
import ThinkingSvg exposing (thinkingSvg)


port saveScores : E.Value -> Cmd msg


type alias Flags =
    { initialSeed : Int, highScores : HighScores }


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


encodeHighScore : Maybe Int -> E.Value
encodeHighScore maybeScore =
    case maybeScore of
        Just score ->
            E.int score

        Nothing ->
            E.null


saveHighScores : HighScores -> Cmd msg
saveHighScores scores =
    saveScores
        (E.object
            [ ( "addition", encodeHighScore scores.addition )
            , ( "additionBig", encodeHighScore scores.additionBig )
            , ( "multiplication", encodeHighScore scores.multiplication )
            , ( "multiplicationBig", encodeHighScore scores.multiplicationBig )
            ]
        )



-- MODEL


type alias Coords =
    { x : Float, y : Float }


type alias DragData =
    { draggedAnswer : Int
    , draggedOverQuestion : Maybe Int
    , mouseCoords : Coords -- The mouse's current position, only tracked and updated if a drag exists
    , offset : Coords -- The offset between the cursor and the top left corner of the dragged thing, for correct position while dragging
    }


type alias HighScores =
    { addition : Maybe Int, additionBig : Maybe Int, multiplication : Maybe Int, multiplicationBig : Maybe Int }


type alias Model =
    { seed : Random.Seed
    , gameState : GameState
    , gameType : Game.GameType
    , highScores : HighScores
    }


type GameState
    = MainMenu
    | GameStarted Game (Maybe DragData)
    | GameOver CompletedGame


init : Flags -> ( Model, Cmd Msg )
init { initialSeed, highScores } =
    ( { seed = Random.initialSeed initialSeed
      , highScores = highScores
      , gameState = MainMenu
      , gameType = Game.GameAddition
      }
    , Cmd.none
    )



-- UPDATE


additionBigUnlocked : HighScores -> Bool
additionBigUnlocked highScores =
    case highScores.addition of
        Just score ->
            score >= config.grades.b

        Nothing ->
            False


multiplicationBigUnlocked : HighScores -> Bool
multiplicationBigUnlocked highScores =
    case highScores.multiplication of
        Just score ->
            score >= config.grades.b

        Nothing ->
            False


type Msg
    = NoOp
    | WithElement String (Browser.Dom.Element -> Msg)
    | HandleAnimationFrameDelta Float
    | HandleStartGameClick
    | HandleAnswerInput Int Game.Answer
    | HandleNextSheetClick
    | HandlePointerDownAnswer Int Pointer.Event Browser.Dom.Element
    | HandlePointerEnterQuestion Int
    | HandlePointerLeaveQuestion Int
    | HandlePointerUpQuestion Int
    | HandlePointerUpWindow
    | HandlePointerMove Pointer.Event
    | HandleStartOverClick
    | HandleGiveUpClick
    | HandleMainMenuClick
    | HandleGameTypeClick Game.GameType


updateHighScores : Game.GameType -> Int -> HighScores -> HighScores
updateHighScores gameType finalScore highScores =
    case gameType of
        Game.GameAddition ->
            case highScores.addition of
                Just additionHighScore ->
                    if finalScore > additionHighScore then
                        { highScores | addition = Just finalScore }

                    else
                        highScores

                Nothing ->
                    { highScores | addition = Just finalScore }

        Game.GameAdditionBig ->
            case highScores.additionBig of
                Just additionBigHighScore ->
                    if finalScore > additionBigHighScore then
                        { highScores | additionBig = Just finalScore }

                    else
                        highScores

                Nothing ->
                    { highScores | additionBig = Just finalScore }

        Game.GameMultiplication ->
            case highScores.multiplication of
                Just multiplicationHighScore ->
                    if finalScore > multiplicationHighScore then
                        { highScores | multiplication = Just finalScore }

                    else
                        highScores

                Nothing ->
                    { highScores | multiplication = Just finalScore }

        Game.GameMultiplicationBig ->
            case highScores.multiplicationBig of
                Just multiplicationBigHighScore ->
                    if finalScore > multiplicationBigHighScore then
                        { highScores | multiplicationBig = Just finalScore }

                    else
                        highScores

                Nothing ->
                    { highScores | multiplicationBig = Just finalScore }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp : ( Model, Cmd Msg )
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleAnimationFrameDelta delta ->
            case model.gameState of
                GameStarted game dragData ->
                    case Game.handleAnimationFrameDelta delta game of
                        Game.GameContinues newGameGenerator ->
                            let
                                ( newGame, newSeed ) =
                                    Random.step newGameGenerator model.seed
                            in
                            ( { model | seed = newSeed, gameState = GameStarted newGame dragData }, Cmd.none )

                        Game.GameEnded completedGame ->
                            let
                                gameSummary : Game.GameSummary
                                gameSummary =
                                    Game.gameSummary completedGame

                                highScores : HighScores
                                highScores =
                                    model.highScores
                            in
                            ( { model | gameState = GameOver completedGame }, saveHighScores (updateHighScores model.gameType gameSummary.finalScore highScores) )

                _ ->
                    noOp

        HandleStartGameClick ->
            case model.gameState of
                MainMenu ->
                    let
                        ( newGame, newSeed ) =
                            Random.step (Game.newGameGenerator model.gameType) model.seed

                        canStartGame : Bool
                        canStartGame =
                            case model.gameType of
                                Game.GameAdditionBig ->
                                    additionBigUnlocked model.highScores

                                Game.GameMultiplicationBig ->
                                    multiplicationBigUnlocked model.highScores

                                _ ->
                                    True
                    in
                    if canStartGame then
                        ( { model | seed = newSeed, gameState = GameStarted newGame Nothing }, Cmd.none )

                    else
                        noOp

                _ ->
                    noOp

        HandleAnswerInput questionIndex answer ->
            case model.gameState of
                GameStarted game dragData ->
                    let
                        newGame : Game
                        newGame =
                            Game.answerQuestion questionIndex answer game
                    in
                    ( { model | gameState = GameStarted newGame dragData }, Cmd.none )

                _ ->
                    noOp

        HandleNextSheetClick ->
            case model.gameState of
                GameStarted game dragData ->
                    let
                        ( newGame, newSeed ) =
                            Random.step (Game.goNextSheetGenerator game) model.seed
                    in
                    ( { model | seed = newSeed, gameState = GameStarted newGame dragData }, Cmd.none )

                _ ->
                    noOp

        HandlePointerDownAnswer index event element ->
            case model.gameState of
                GameStarted game _ ->
                    let
                        ( pointerX, pointerY ) =
                            event.pointer.clientPos

                        coords : Coords
                        coords =
                            { x = pointerX, y = pointerY }

                        offset : Coords
                        offset =
                            { x = pointerX - element.element.x
                            , y = pointerY - element.element.y
                            }

                        newDragData : DragData
                        newDragData =
                            { draggedAnswer = index, draggedOverQuestion = Nothing, mouseCoords = coords, offset = offset }
                    in
                    ( { model | gameState = GameStarted game (Just newDragData) }, Cmd.none )

                _ ->
                    noOp

        HandlePointerEnterQuestion index ->
            case model.gameState of
                GameStarted game (Just dragData) ->
                    let
                        newDragData : DragData
                        newDragData =
                            { dragData | draggedOverQuestion = Just index }
                    in
                    ( { model | gameState = GameStarted game (Just newDragData) }, Cmd.none )

                _ ->
                    noOp

        HandlePointerLeaveQuestion _ ->
            case model.gameState of
                GameStarted game (Just dragData) ->
                    let
                        newDragData : DragData
                        newDragData =
                            { dragData | draggedOverQuestion = Nothing }
                    in
                    ( { model | gameState = GameStarted game (Just newDragData) }, Cmd.none )

                _ ->
                    noOp

        HandlePointerUpQuestion index ->
            case model.gameState of
                GameStarted game (Just dragData) ->
                    let
                        newGame : Game
                        newGame =
                            Game.answerQuestion index dragData.draggedAnswer game
                    in
                    ( { model | gameState = GameStarted newGame Nothing }, Cmd.none )

                _ ->
                    noOp

        HandlePointerUpWindow ->
            case model.gameState of
                GameStarted game _ ->
                    ( { model | gameState = GameStarted game Nothing }, Cmd.none )

                _ ->
                    noOp

        HandlePointerMove event ->
            case model.gameState of
                GameStarted game (Just dragData) ->
                    let
                        ( clientX, clientY ) =
                            event.pointer.clientPos

                        coords : Coords
                        coords =
                            { x = clientX, y = clientY }

                        newDragData : DragData
                        newDragData =
                            { dragData | mouseCoords = coords }
                    in
                    ( { model | gameState = GameStarted game (Just newDragData) }, Cmd.none )

                _ ->
                    noOp

        HandleStartOverClick ->
            case model.gameState of
                GameStarted game _ ->
                    let
                        ( newGame, newSeed ) =
                            Random.step (Game.newGameGenerator game.gameType) model.seed
                    in
                    ( { model | seed = newSeed, gameState = GameStarted newGame Nothing }, Cmd.none )

                _ ->
                    noOp

        HandleGiveUpClick ->
            case model.gameState of
                GameStarted game _ ->
                    let
                        completedGame : Game.CompletedGame
                        completedGame =
                            Game.completeGame game

                        gameSummary : Game.GameSummary
                        gameSummary =
                            Game.gameSummary completedGame

                        highScores : HighScores
                        highScores =
                            model.highScores

                        newHighScores : HighScores
                        newHighScores =
                            updateHighScores model.gameType gameSummary.finalScore highScores
                    in
                    ( { model | gameState = GameOver completedGame, highScores = newHighScores }, saveHighScores newHighScores )

                _ ->
                    noOp

        HandleMainMenuClick ->
            ( { model | gameState = MainMenu }, Cmd.none )

        WithElement id messageNeedingElement ->
            let
                handleGetElementResult : Result Browser.Dom.Error Browser.Dom.Element -> Msg
                handleGetElementResult res =
                    res
                        |> Result.map messageNeedingElement
                        |> Result.withDefault NoOp
            in
            ( model
            , Task.attempt handleGetElementResult (Browser.Dom.getElement id)
            )

        HandleGameTypeClick gameType ->
            case model.gameState of
                MainMenu ->
                    ( { model | gameType = gameType }, Cmd.none )

                _ ->
                    noOp



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyDownDecoder : D.Decoder Msg
        keyDownDecoder =
            D.field "key" D.string
                |> D.map
                    (\str ->
                        if str == " " then
                            HandleNextSheetClick

                        else
                            NoOp
                    )
    in
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta HandleAnimationFrameDelta
        , Browser.Events.onKeyDown keyDownDecoder
        ]



-- VIEW


answerId : Int -> String
answerId index =
    "answer-" ++ String.fromInt index


proseClass : Attribute msg
proseClass =
    class "prose prose-sm md:prose-base"


mainMenuView : Model -> Html Msg
mainMenuView model =
    let
        prettyGood : Html Msg
        prettyGood =
            div [ class "badge badge-info" ] [ text "Pretty Good" ]

        maybeHighScore : Maybe Int
        maybeHighScore =
            case model.gameType of
                Game.GameAddition ->
                    model.highScores.addition

                Game.GameAdditionBig ->
                    model.highScores.additionBig

                Game.GameMultiplication ->
                    model.highScores.multiplication

                Game.GameMultiplicationBig ->
                    model.highScores.multiplicationBig

        highGrade : String
        highGrade =
            case maybeHighScore of
                Nothing ->
                    "N/A"

                Just highScore ->
                    if highScore >= config.grades.a then
                        "A"

                    else if highScore >= config.grades.b then
                        "B"

                    else if highScore >= config.grades.c then
                        "C"

                    else if highScore >= config.grades.d then
                        "D"

                    else
                        "F"

        canStartGame : Bool
        canStartGame =
            case model.gameType of
                Game.GameAdditionBig ->
                    additionBigUnlocked model.highScores

                Game.GameMultiplicationBig ->
                    multiplicationBigUnlocked model.highScores

                _ ->
                    True
    in
    div [ class "w-full h-full flex flex-col gap-4 lg:gap-12 items-center" ]
        [ div [ proseClass ]
            [ h1 [ class "flex items-center text-2xl" ]
                [ span [ class "font-bold" ] [ text "Pretty Good Math v3" ]
                ]
            , p []
                [ text "Welcome to Pretty Good Math, this is your final exam!"
                ]
            , ol []
                [ li [] [ text "Drag and drop ", strong [] [ text "answers" ], text " onto ", strong [] [ text "questions" ] ]
                , li [] [ text "Remember that math isn't about being perfect! ", prettyGood, text " is good enough :)" ]
                , li [] [ text "Click ", span [ class "badge" ] [ text "Next Sheet" ], text " when you're ready for the next page of problems" ]
                ]
            , p [] [ text "You have ", strong [] [ text "1 minute" ], text " to answer as many questions as you can. Good luck!" ]
            ]
        , div [ class "flex items-center gap-4" ]
            (Game.gameTypes
                |> List.map
                    (\gt ->
                        let
                            questionTypeStats : Math.QuestionTypeStats
                            questionTypeStats =
                                Game.getStats gt
                        in
                        label
                            [ class "flex items-center gap-1 cursor-pointer"
                            , Pointer.onDown (\_ -> HandleGameTypeClick gt)
                            ]
                            [ text questionTypeStats.title
                            , input
                                [ type_ "radio"
                                , class "radio"
                                , checked (model.gameType == gt)
                                ]
                                []
                            ]
                    )
            )
        , div [ classList [ ( "hidden", not canStartGame ) ], class "h-[42px]" ]
            [ strong [ classList [ ( "invisible", highGrade == "N/A" ) ], class "underline border rounded border-black p-2" ] [ text highGrade ]
            ]
        , div [ classList [ ( "hidden", canStartGame ) ], class "flex items-center gap-2 h-[42px]" ]
            [ FeatherIcons.lock
                |> FeatherIcons.withSize 24
                |> FeatherIcons.toHtml []
            , text "Earn"
            , strong [ class "underline border rounded border-black p-2" ] [ text "B" ]
            , text "or higher in the previous test"
            ]
        , button
            [ class "btn btn-primary px-16 disabled:cursor-not-allowed"
            , onClick HandleStartGameClick
            , disabled (not canStartGame)
            ]
            [ text "Start!" ]
        ]


renderScore : Math.Score -> Html Msg
renderScore score =
    let
        ( scoreText, badgeClass ) =
            case score of
                Math.Perfect ->
                    ( "Perfect", class "badge-success" )

                Math.PrettyGood ->
                    ( "Pretty Good", class "badge-info" )

                Math.Sure ->
                    ( "Sure", class "badge-warning" )

                Math.WhatTheHeck ->
                    ( "What the heck?", class "badge-error" )
    in
    div [ class "badge animate-fadeInUp", badgeClass ]
        [ text scoreText ]


gameView : Game -> Maybe DragData -> Html Msg
gameView game maybeDragData =
    let
        renderQuestion : Int -> Math.Question -> Html Msg
        renderQuestion index question =
            case question of
                Math.Addition left right ->
                    div
                        [ class "h-12 flex items-center gap-2"
                        ]
                        [ span [] [ text (String.fromInt left) ]
                        , span []
                            [ FeatherIcons.plus
                                |> FeatherIcons.withSize 16
                                |> FeatherIcons.toHtml []
                            ]
                        , span [] [ text (String.fromInt right) ]
                        , span [] [ text "=" ]
                        ]

                Math.AdditionBig left right ->
                    div
                        [ class "h-12 flex items-center gap-2"
                        ]
                        [ span [] [ text (String.fromInt left) ]
                        , span []
                            [ FeatherIcons.plus
                                |> FeatherIcons.withSize 16
                                |> FeatherIcons.toHtml []
                            ]
                        , span [] [ text (String.fromInt right) ]
                        , span [] [ text "=" ]
                        ]

                Math.Multiplication left right ->
                    div
                        [ class "h-12 flex items-center gap-2"
                        ]
                        [ span [] [ text (String.fromInt left) ]
                        , span []
                            [ FeatherIcons.x
                                |> FeatherIcons.withSize 16
                                |> FeatherIcons.toHtml []
                            ]
                        , span [] [ text (String.fromInt right) ]
                        , span [] [ text "=" ]
                        ]

                Math.MultiplicationBig left right ->
                    div
                        [ class "h-12 flex items-center gap-2"
                        ]
                        [ span [] [ text (String.fromInt left) ]
                        , span []
                            [ FeatherIcons.x
                                |> FeatherIcons.withSize 16
                                |> FeatherIcons.toHtml []
                            ]
                        , span [] [ text (String.fromInt right) ]
                        , span [] [ text "=" ]
                        ]

        renderQuestionAnswerPair : Int -> Game.QuestionAnswerPair -> Html Msg
        renderQuestionAnswerPair questionIndex questionAnswer =
            let
                rowStyles : Attribute Msg
                rowStyles =
                    class "w-full h-14 flex items-center px-8"
            in
            case questionAnswer of
                Game.Unanswered question ->
                    let
                        isDragInProgress : Bool
                        isDragInProgress =
                            case maybeDragData of
                                Nothing ->
                                    False

                                Just _ ->
                                    True

                        isDraggedOver : Bool
                        isDraggedOver =
                            case maybeDragData of
                                Nothing ->
                                    False

                                Just { draggedOverQuestion } ->
                                    draggedOverQuestion == Just questionIndex
                    in
                    li
                        [ rowStyles
                        , class "gap-2"
                        , Pointer.onWithOptions "pointerenter"
                            { preventDefault = True, stopPropagation = False }
                            (\_ ->
                                HandlePointerEnterQuestion questionIndex
                            )
                        , Pointer.onWithOptions "pointerleave"
                            { preventDefault = True, stopPropagation = False }
                            (\_ -> HandlePointerLeaveQuestion questionIndex)
                        , Pointer.onWithOptions "pointerup"
                            { preventDefault = True, stopPropagation = False }
                            (\_ -> HandlePointerUpQuestion questionIndex)
                        ]
                        [ div
                            [ class "h-12 flex items-center gap-2 p-4 border border-dashed border-2 rounded-lg border-neutral border-opacity-0"
                            , classList [ ( "border-primary border-opacity-100", isDraggedOver ), ( "border-neutral border-opacity-50", isDragInProgress && not isDraggedOver ) ]
                            ]
                            [ renderQuestion questionIndex question
                            , span [] [ text "_____" ]
                            ]
                        ]

                Game.Answered question answer ->
                    li [ rowStyles, class "gap-4" ]
                        [ div [ class "h-12 flex items-center gap-2 p-4 opacity-50" ]
                            [ renderQuestion questionIndex question
                            , span [ class "font-bold" ] [ text (String.fromInt answer) ]
                            ]
                        , div [ class "opacity" ] [ renderScore (Math.scoreQuestion question answer) ]
                        ]

        answerDimensionClass : Attribute Msg
        answerDimensionClass =
            class "w-24 h-12"

        renderAnswer : Int -> Game.Answer -> Html Msg
        renderAnswer index answer =
            let
                dragAttrs : List (Attribute Msg)
                dragAttrs =
                    case maybeDragData of
                        Nothing ->
                            [ class "cursor-move animate-fadeInThenFloat"
                            ]

                        Just { draggedAnswer, mouseCoords, offset } ->
                            if draggedAnswer == index then
                                [ class "fixed shadow-lg z-10 pointer-events-none"
                                , style "top" (String.fromFloat (mouseCoords.y - offset.y) ++ "px")
                                , style "left" (String.fromFloat (mouseCoords.x - offset.x) ++ "px")
                                ]

                            else
                                [ class "animate-fadeInThenFloat" ]
            in
            -- This extra div is to occupy space while a drag is in progress
            div
                [ answerDimensionClass
                , id (answerId index)
                ]
                [ div
                    ([ answerDimensionClass
                     , class "rounded flex items-center justify-center border border-neutral bg-base-100 animate-fadeInThenFloat"
                     , Pointer.onWithOptions "pointerdown"
                        { stopPropagation = False, preventDefault = True }
                        (\event ->
                            HandlePointerDownAnswer index event
                                |> WithElement (answerId index)
                        )
                     ]
                        ++ dragAttrs
                    )
                    [ span [] [ text (String.fromInt answer) ] ]
                ]

        renderTimer : Html Msg
        renderTimer =
            let
                percentTimeLeft : Float
                percentTimeLeft =
                    Game.percentTimeLeft game
            in
            progress [ class "progress progress-primary w-56", attribute "value" (String.fromFloat (percentTimeLeft * 100)), attribute "max" "100" ] []

        getOffsetStyles : Int -> List (Attribute Msg)
        getOffsetStyles index =
            [ style "top" (String.fromInt (index * 4) ++ "px")
            , style "left" (String.fromInt (index * 2) ++ "px")
            ]

        sheetStyles : Attribute Msg
        sheetStyles =
            class "w-[355px] h-[440px] bg-base-100 absolute shadow-xl bg-base-100 border border-gray-500"

        placeholderSheet : Int -> Html Msg
        placeholderSheet index =
            div
                ([ sheetStyles
                 , classList [ ( "hidden", not (index < List.length game.completedSheets + 4) ) ]
                 ]
                    ++ getOffsetStyles index
                )
                []

        renderCurrentSheet : Html Msg
        renderCurrentSheet =
            div ([ sheetStyles, class "flex flex-col items-center gap-6 py-16 animate-slideRotate" ] ++ getOffsetStyles (List.length game.completedSheets))
                [ div [ class "text-xl font-bold" ] [ text "Question sheet" ]
                , ul [ class "w-full flex flex-col items-start" ]
                    (List.indexedMap renderQuestionAnswerPair game.currentSheet)
                ]

        allSheets : Html Msg
        allSheets =
            div [ class "relative" ]
                (List.indexedMap (\index _ -> placeholderSheet index) game.completedSheets
                    ++ [ renderCurrentSheet ]
                )
    in
    div [ class "w-full h-full flex flex-col gap-4" ]
        [ div [ class "flex justify-center items-center relative" ]
            [ renderTimer
            , div [ class "flex items-center gap-8 h-full absolute top-0 right-0" ]
                [ button [ class "btn btn-sm btn-error", onClick HandleGiveUpClick ] [ text "Give up" ]
                ]
            ]
        , div [ class "w-full grid grid-cols-2 gap-4" ]
            [ -- Answers
              div [ class "flex flex-col items-end gap-6 p-4 xl:p-16" ]
                [ div [ class "text-xl font-bold" ] [ text "Answers" ]
                , ul [ class "grid grid-cols-2 lg:grid-cols-5 gap-4 min-h-[48px]", style "direction" "rtl" ]
                    (List.indexedMap renderAnswer game.answers)
                , div [ class "w-600px max-w-[600px]" ]
                    [ thinkingSvg
                    ]
                ]

            -- Questions
            , div [ class "flex justify-start" ]
                [ div [ class "flex flex-col items-start gap-4 p-16" ]
                    [ button [ class "btn", classList [ ( "btn-primary", Game.allQuestionsAnswered game ) ], onClick HandleNextSheetClick ] [ text "Next Sheet" ]
                    , allSheets
                    ]
                ]
            ]
        ]


gameOverView : Game.GameType -> CompletedGame -> Html Msg
gameOverView gameType completedGame =
    let
        gameSummary : Game.GameSummary
        gameSummary =
            Game.gameSummary completedGame

        playerWon : Bool
        playerWon =
            gameSummary.finalScore >= config.passPoints

        questionTypeStats : Math.QuestionTypeStats
        questionTypeStats =
            Game.getStats gameType
    in
    div [ class "w-full flex justify-center", class "animate-fadeInUp" ]
        [ div [ class "max-w-3xl shadow-xl rounded flex flex-col gap-4 lg:gap-16 p-12 items-center overflow-hidden" ]
            [ div [ class "prose prose-sm md:prose-base" ] [ h1 [] [ text "Results" ] ]
            , div [ class "prose prose-sm md:prose-base" ] [ p [] [ text "Test type: ", strong [] [ text questionTypeStats.title ] ] ]
            , div [] [ renderGameSummary gameType gameSummary ]
            , div [ class "prose prose-sm md:prose-base" ]
                [ text "Final Score: "
                , strong [] [ text (String.fromInt gameSummary.finalScore) ]
                , text " / "
                , strong [] [ text (String.fromInt config.passPoints) ]
                , text " points"
                ]
            , if playerWon then
                div [ class "bg-success/50 text-success-content p-4 rounded border-2 border-success animate-popWiggle" ] [ text "Result: You passed!" ]

              else
                div [ class "bg-error/50 text-success-error p-4 rounded border-2 border-error animate-popWiggle" ] [ text "Result: Failed to pass" ]
            , div [ class "w-full flex justify-center items-center" ] [ button [ class "btn btn-primary", onClick HandleMainMenuClick ] [ text "Main Menu" ] ]
            ]
        ]


scoresForTable : Game.GameType -> List ( Math.Score, Int, Maybe Int )
scoresForTable questionType =
    let
        questionTypeStats : Math.QuestionTypeStats
        questionTypeStats =
            Game.getStats questionType
    in
    [ ( Math.Perfect, Game.scoreToPoints Math.Perfect, Just 0 )
    , ( Math.PrettyGood, Game.scoreToPoints Math.PrettyGood, Just questionTypeStats.margins.prettyGood )
    , ( Math.Sure, Game.scoreToPoints Math.Sure, Just questionTypeStats.margins.sure )
    , ( Math.WhatTheHeck, Game.scoreToPoints Math.WhatTheHeck, Nothing )
    ]


renderRubric : Game.GameType -> Html Msg
renderRubric gameType =
    table [ class "table w-[500px]" ]
        [ thead []
            [ tr []
                [ th [] [ text "Score" ]
                , th [] [ text "Margin of error" ]
                , th [] [ text "Points" ]
                ]
            ]
        , tbody []
            (scoresForTable gameType
                |> List.map
                    (\( score, points, maybeMargin ) ->
                        tr []
                            [ td [] [ renderScore score ]
                            , case maybeMargin of
                                Nothing ->
                                    td [ class "font-thin" ] [ text "--" ]

                                Just margin ->
                                    td [ class "flex items-center gap-1 font-light" ]
                                        [ span [] [ text "+/-" ]
                                        , span [] [ text (String.fromInt margin) ]
                                        ]
                            , td [ class "font-semibold" ] [ text (String.fromInt points) ]
                            ]
                    )
            )
        ]


renderGameSummary : Game.GameType -> Game.GameSummary -> Html Msg
renderGameSummary gameType gameSummary =
    table [ class "table w-[500px]" ]
        [ thead []
            [ tr []
                [ th [] [ text "Score" ]
                , th [] [ text "Points" ]
                , th [] [ text "Subtotal" ]
                ]
            ]
        , tbody []
            (scoresForTable gameType
                |> List.map
                    (\( score, points, _ ) ->
                        let
                            count : Int
                            count =
                                Game.getByScore score gameSummary.scoreCounts

                            subtotal : Int
                            subtotal =
                                Game.scoreToPoints score * count
                        in
                        tr []
                            [ td [ class "flex items-center gap-1" ]
                                [ renderScore score
                                , span [ class "flex items-center gap-1 text-opacity-50" ]
                                    [ FeatherIcons.x |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                                    , text (String.fromInt count)
                                    ]
                                ]
                            , td [] [ text (String.fromInt points) ]
                            , td [ class "font-semibold" ] [ text (String.fromInt subtotal) ]
                            ]
                    )
            )
        ]


view : Model -> Html Msg
view model =
    let
        dragInProgress : Bool
        dragInProgress =
            case model.gameState of
                GameStarted _ (Just _) ->
                    True

                _ ->
                    False
    in
    div
        ([ class "w-screen h-screen overflow-auto p-4 lg:p-16 pb-4"
         , Pointer.onUp (\_ -> HandlePointerUpWindow)
         , Pointer.onWithOptions "pointermove"
            { stopPropagation = False, preventDefault = True }
            HandlePointerMove
         , classList [ ( "cursor-none", dragInProgress ) ]
         ]
            ++ (if dragInProgress then
                    [ style "touch-action" "none"
                    ]

                else
                    []
               )
        )
        [ case model.gameState of
            MainMenu ->
                mainMenuView model

            GameStarted game maybeDragData ->
                gameView game maybeDragData

            GameOver completedGame ->
                gameOverView model.gameType completedGame
        ]
