module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Config exposing (config)
import FeatherIcons
import Game exposing (CompletedGame, Game)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as D
import List.Extra
import Maybe.Extra
import Random
import Task
import ThinkingSvg exposing (thinkingSvg)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Coords =
    { x : Float, y : Float }


type alias DragData =
    { draggedAnswer : Int
    , draggedOverQuestion : Maybe Int
    , mouseCoords : Coords -- The mouse's current position, only tracked and updated if a drag exists
    , offset : Coords -- The offset between the cursor and the top left corner of the dragged thing, for correct position while dragging
    }


type Model
    = MainMenu Random.Seed
    | GameStarted Random.Seed Game (Maybe DragData)
    | GameOver Random.Seed CompletedGame


init : () -> ( Model, Cmd Msg )
init _ =
    ( MainMenu (Random.initialSeed 0), Cmd.none )



-- UPDATE


coordDecoder : D.Decoder Coords
coordDecoder =
    D.map2
        Coords
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)


type Msg
    = NoOp
    | WithElement String (Browser.Dom.Element -> Msg)
    | HandleAnimationFrameDelta Float
    | HandleStartGameClick
    | HandleAnswerInput Int Game.Answer
    | HandleNextSheetClick
    | HandleMouseDownAnswer Int Coords Browser.Dom.Element
    | HandleMouseOverQuestion Int
    | HandleMouseOutQuestion Int
    | HandleMouseUpQuestion Int
    | HandleMouseUpWindow
    | HandleMouseMove Coords
    | HandleStartOverClick
    | HandleGiveUpClick
    | HandleMainMenuClick


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
            case model of
                GameStarted seed game dragData ->
                    case Game.updateTimer delta game of
                        Game.TimeLeft newGame ->
                            ( GameStarted seed newGame dragData, Cmd.none )

                        Game.TimeUp completedGame ->
                            ( GameOver seed completedGame, Cmd.none )

                _ ->
                    noOp

        HandleStartGameClick ->
            case model of
                MainMenu seed ->
                    let
                        ( newGame, newSeed ) =
                            Random.step Game.gameGenerator seed
                    in
                    ( GameStarted newSeed newGame Nothing, Cmd.none )

                _ ->
                    noOp

        HandleAnswerInput questionIndex answer ->
            case model of
                GameStarted seed game dragData ->
                    let
                        newGame : Game
                        newGame =
                            Game.answerQuestion questionIndex answer game
                    in
                    ( GameStarted seed newGame dragData, Cmd.none )

                _ ->
                    noOp

        HandleNextSheetClick ->
            case model of
                GameStarted seed game dragData ->
                    let
                        ( newGame, newSeed ) =
                            Random.step (Game.goNextSheetGenerator game) seed
                    in
                    ( GameStarted newSeed newGame dragData, Cmd.none )

                _ ->
                    noOp

        HandleMouseDownAnswer index coords element ->
            case model of
                GameStarted seed game _ ->
                    let
                        offset : Coords
                        offset =
                            { x = coords.x - element.element.x
                            , y = coords.y - element.element.y
                            }

                        newDragData : DragData
                        newDragData =
                            { draggedAnswer = index, draggedOverQuestion = Nothing, mouseCoords = coords, offset = offset }
                    in
                    ( GameStarted seed game (Just newDragData), Cmd.none )

                _ ->
                    noOp

        HandleMouseOverQuestion index ->
            case model of
                GameStarted seed game (Just dragData) ->
                    let
                        newDragData : DragData
                        newDragData =
                            { dragData | draggedOverQuestion = Just index }
                    in
                    ( GameStarted seed game (Just newDragData), Cmd.none )

                _ ->
                    noOp

        HandleMouseOutQuestion _ ->
            case model of
                GameStarted seed game (Just dragData) ->
                    let
                        newDragData : DragData
                        newDragData =
                            { dragData | draggedOverQuestion = Nothing }
                    in
                    ( GameStarted seed game (Just newDragData), Cmd.none )

                _ ->
                    noOp

        HandleMouseUpQuestion index ->
            case model of
                GameStarted seed game (Just dragData) ->
                    let
                        newGame : Game
                        newGame =
                            Game.answerQuestion index dragData.draggedAnswer game
                    in
                    ( GameStarted seed newGame Nothing, Cmd.none )

                _ ->
                    noOp

        HandleMouseUpWindow ->
            case model of
                GameStarted seed game _ ->
                    ( GameStarted seed game Nothing, Cmd.none )

                _ ->
                    noOp

        HandleMouseMove coords ->
            case model of
                GameStarted seed game (Just dragData) ->
                    let
                        newDragData : DragData
                        newDragData =
                            { dragData | mouseCoords = coords }
                    in
                    ( GameStarted seed game (Just newDragData), Cmd.none )

                _ ->
                    noOp

        HandleStartOverClick ->
            case model of
                GameStarted seed _ _ ->
                    let
                        ( newGame, newSeed ) =
                            Random.step Game.gameGenerator seed
                    in
                    ( GameStarted newSeed newGame Nothing, Cmd.none )

                _ ->
                    noOp

        HandleGiveUpClick ->
            case model of
                GameStarted seed game _ ->
                    ( GameOver seed (Game.completeGame game), Cmd.none )

                _ ->
                    noOp

        HandleMainMenuClick ->
            let
                newSeed : Random.Seed
                newSeed =
                    case model of
                        MainMenu seed ->
                            seed

                        GameStarted seed _ _ ->
                            seed

                        GameOver seed _ ->
                            seed
            in
            ( MainMenu newSeed, Cmd.none )

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
        , Browser.Events.onMouseMove (D.map HandleMouseMove coordDecoder)
        , Browser.Events.onKeyDown keyDownDecoder
        ]



-- VIEW


answerId : Int -> String
answerId index =
    "answer-" ++ String.fromInt index


mainMenuView : Html Msg
mainMenuView =
    div [ class "w-full h-full flex flex-col gap-4 lg:gap-16 items-center" ]
        [ div [ class "prose prose-sm md:prose-base" ]
            [ h1 [ class "flex items-center text-2xl" ]
                [ span [ class "font-bold" ] [ text "Pretty Good Math" ]
                ]
            , p []
                [ text "Welcome to Pretty Good Math, this is your final exam!"
                ]
            , ol []
                [ li [] [ text "Drag and drop ", strong [] [ text "answers" ], text " onto ", strong [] [ text "questions" ] ]
                , li [] [ text "Remember that math isn't about being perfect! ", renderScore Game.PrettyGood, text " is good enough :)" ]
                , li [] [ text "Click ", span [ class "badge" ] [ text "Next Sheet" ], text " when you're ready for the next page of problems" ]
                ]
            , p [] [ text "You have ", strong [] [ text "30 seconds" ], text " to answer as many questions as you can. Good luck!" ]
            ]
        , button [ class "btn btn-primary px-16", onClick HandleStartGameClick ] [ text "Start!" ]
        , div [ class "divider" ] []
        , div [ class "prose prose-sm md:prose-base" ] [ h2 [] [ text "Grading" ] ]
        , div [ class "prose prose-sm md:prose-base" ] [ p [] [ text "Passing: ", strong [] [ text (String.fromInt config.passPoints) ], text " points" ] ]
        , renderRubric
        ]


renderScore : Game.Score -> Html Msg
renderScore score =
    let
        ( scoreText, badgeClass ) =
            case score of
                Game.Perfect ->
                    ( "Perfect", class "badge-success" )

                Game.PrettyGood ->
                    ( "Pretty Good", class "badge-info" )

                Game.Sure ->
                    ( "Sure", class "badge-warning" )

                Game.WhatTheHeck ->
                    ( "What the heck?", class "badge-error" )
    in
    div [ class "badge", badgeClass ]
        [ text scoreText ]


gameView : Game -> Maybe DragData -> Html Msg
gameView game maybeDragData =
    let
        renderQuestion : Int -> Game.Question -> Html Msg
        renderQuestion index question =
            case question of
                Game.Addition left right ->
                    div
                        [ class "h-12 flex items-center gap-2"
                        ]
                        [ span [] [ text (String.fromInt left) ]
                        , span [] [ text "+" ]
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
                        , preventDefaultOn "mouseover" (D.succeed ( HandleMouseOverQuestion questionIndex, True ))
                        , preventDefaultOn "mouseout" (D.succeed ( HandleMouseOutQuestion questionIndex, True ))
                        , preventDefaultOn "mouseup" (D.succeed ( HandleMouseUpQuestion questionIndex, True ))
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
                        , div [ class "opacity" ] [ renderScore (Game.scoreQuestion question answer) ]
                        ]

        answerDimensionClass : Attribute Msg
        answerDimensionClass =
            class "w-24 h-12"

        renderAnswer : Int -> Game.Answer -> Html Msg
        renderAnswer index answer =
            let
                mouseDownDecoder : D.Decoder ( Msg, Bool )
                mouseDownDecoder =
                    coordDecoder
                        |> D.map
                            (\coords ->
                                ( HandleMouseDownAnswer index coords
                                    |> WithElement (answerId index)
                                , True
                                )
                            )

                dragAttrs : List (Attribute Msg)
                dragAttrs =
                    case maybeDragData of
                        Nothing ->
                            [ class "cursor-move animate-float", style "animation-delay" (String.fromInt (modBy 1000 (index * 200)) ++ "ms") ]

                        Just { draggedAnswer, mouseCoords, offset } ->
                            if draggedAnswer == index then
                                [ class "fixed shadow-lg z-10"
                                , style "pointer-events" "none"
                                , style "top" (String.fromFloat (mouseCoords.y - offset.y) ++ "px")
                                , style "left" (String.fromFloat (mouseCoords.x - offset.x) ++ "px")
                                ]

                            else
                                [ class "animate-float" ]
            in
            -- This extra div is to occupy space while a drag is in progress
            div
                [ answerDimensionClass
                , id (answerId index)
                ]
                [ div
                    ([ answerDimensionClass
                     , class "rounded flex items-center justify-center border border-neutral bg-base-100"
                     , preventDefaultOn "mousedown" mouseDownDecoder
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
            class "w-[350px] h-[440px] bg-base-100 absolute shadow-xl bg-base-100 border border-gray-500"

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
        [ div [ class "flex justify-center items-center gap-8" ]
            [ button [ class "btn btn-sm btn-outline", onClick HandleStartOverClick ] [ text "Start Over" ]
            , renderTimer
            , button [ class "btn btn-sm btn-error", onClick HandleGiveUpClick ] [ text "Give up" ]
            ]
        , div [ class "w-full grid grid-cols-2 gap-4" ]
            [ -- Answers
              div [ class "flex flex-col items-end gap-6 p-4 xl:p-16" ]
                [ div [ class "text-xl font-bold" ] [ text "Answers" ]
                , ul [ class "grid grid-cols-2 lg:grid-cols-5 gap-4" ]
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


gameOverView : CompletedGame -> Html Msg
gameOverView completedGame =
    let
        gameSummary : Game.GameSummary
        gameSummary =
            Game.gameSummary completedGame

        playerWon : Bool
        playerWon =
            gameSummary.finalScore >= config.passPoints
    in
    div [ class "w-full flex justify-center", class "animate-fadeInUp" ]
        [ div [ class "max-w-3xl shadow-xl rounded flex flex-col gap-4 lg:gap-16 p-12 items-center overflow-hidden" ]
            [ div [ class "prose prose-sm md:prose-base" ] [ h1 [] [ text "Results" ] ]
            , div [] [ renderGameSummary gameSummary ]
            , div [ class "prose prose-sm md:prose-base" ]
                [ text "Final Score: "
                , strong [] [ text (String.fromInt gameSummary.finalScore) ]
                , text " / "
                , strong [] [ text (String.fromInt config.passPoints) ]
                , text " points"
                ]
            , if playerWon then
                div [ class "bg-success/50 text-success-content p-4 rounded border-2 border-success" ] [ text "Result: You passed!" ]

              else
                div [ class "bg-error/50 text-success-error p-4 rounded border-2 border-error" ] [ text "Result: Failed to pass" ]
            , div [ class "w-full flex justify-center items-center" ] [ button [ class "btn btn-primary", onClick HandleMainMenuClick ] [ text "Main Menu" ] ]
            ]
        ]


scoresForTable : List ( Game.Score, Int, Maybe Int )
scoresForTable =
    [ ( Game.Perfect, Game.scoreToPoints Game.Perfect, Just 0 )
    , ( Game.PrettyGood, Game.scoreToPoints Game.PrettyGood, Just config.prettyGoodMargin )
    , ( Game.Sure, Game.scoreToPoints Game.Sure, Just config.sureMargin )
    , ( Game.WhatTheHeck, Game.scoreToPoints Game.WhatTheHeck, Nothing )
    ]


renderRubric : Html Msg
renderRubric =
    table [ class "table w-[500px]" ]
        [ thead []
            [ tr []
                [ th [] [ text "Score" ]
                , th [] [ text "Margin of error" ]
                , th [] [ text "Points" ]
                ]
            ]
        , tbody []
            (scoresForTable
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


renderGameSummary : Game.GameSummary -> Html Msg
renderGameSummary gameSummary =
    table [ class "table w-[500px]" ]
        [ thead []
            [ tr []
                [ th [] [ text "Score" ]
                , th [] [ text "Points" ]
                , th [] [ text "Count" ]
                , th [] [ text "Subtotal" ]
                ]
            ]
        , tbody []
            (scoresForTable
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
                            [ td [] [ renderScore score ]
                            , td [] [ text (String.fromInt points) ]
                            , td [] [ text (String.fromInt count) ]
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
            case model of
                GameStarted _ _ (Just _) ->
                    True

                _ ->
                    False
    in
    div
        [ class "w-screen h-screen overflow-auto p-4 lg:p-16 pb-4"
        , onMouseUp HandleMouseUpWindow
        , classList [ ( "cursor-none", dragInProgress ) ]
        ]
        [ case model of
            MainMenu _ ->
                mainMenuView

            GameStarted _ game maybeDragData ->
                gameView game maybeDragData

            GameOver _ completedGame ->
                gameOverView completedGame
        ]