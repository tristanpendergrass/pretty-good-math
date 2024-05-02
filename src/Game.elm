module Game exposing (..)

import Config exposing (config)
import Html.Attributes exposing (download)
import List.Extra
import Random


type Score
    = Perfect
    | PrettyGood
    | Sure
    | WhatTheHeck


type Question
    = Addition Int Int


type alias Answer =
    Int


type QuestionAnswerPair
    = Answered Question Answer
    | Unanswered Question


type alias Sheet =
    List QuestionAnswerPair


type alias Game =
    { completedSheets : List Sheet
    , currentSheet : Sheet
    , answers : List Answer
    , timeLeft : Float
    , newAnswerTimeLeft : Float
    }


type alias CompletedGame =
    { completedSheets : List Sheet
    }



-- Generators


addend : Random.Generator Int
addend =
    Random.int config.addendLowerBound config.addendUpperBound


questionGenerator : Random.Generator Question
questionGenerator =
    Random.map2 Addition addend addend


answerGenerator : Random.Generator Answer
answerGenerator =
    Random.int
        (config.addendLowerBound + config.addendLowerBound)
        (config.addendUpperBound + config.addendUpperBound)


sheetGenerator : Random.Generator Sheet
sheetGenerator =
    Random.list config.questionsPerSheet (Random.map Unanswered questionGenerator)


gameGenerator : Random.Generator Game
gameGenerator =
    Random.map
        (\initialSheet ->
            { completedSheets = []
            , currentSheet = initialSheet
            , answers = []
            , timeLeft = config.roundDuration

            -- For his first answer we don't make it random but rather make it happen right away to get the game going
            , newAnswerTimeLeft = config.newAnswerTimeFast.lowerBound
            }
        )
        sheetGenerator


goNextSheetGenerator : Game -> Random.Generator Game
goNextSheetGenerator game =
    Random.map
        (\newSheet ->
            { game | currentSheet = newSheet, answers = [], completedSheets = game.currentSheet :: game.completedSheets }
        )
        sheetGenerator



-- Utils


updateCompletedSheets : (List Sheet -> List Sheet) -> Game -> Game
updateCompletedSheets fn game =
    { game | completedSheets = fn game.completedSheets }


setCurrentSheet : Sheet -> Game -> Game
setCurrentSheet sheet game =
    { game | currentSheet = sheet }


updateCurrentSheet : (Sheet -> Sheet) -> Game -> Game
updateCurrentSheet fn game =
    { game | currentSheet = fn game.currentSheet }


updateAnswers : (List Answer -> List Answer) -> Game -> Game
updateAnswers fn game =
    { game | answers = fn game.answers }


answerQuestion : Int -> Int -> Game -> Game
answerQuestion questionIndex answerIndex game =
    case List.Extra.getAt answerIndex game.answers of
        Nothing ->
            game

        Just answer ->
            game
                |> updateCurrentSheet
                    (List.Extra.updateAt questionIndex
                        (\questionAnswerPair ->
                            case questionAnswerPair of
                                Answered _ _ ->
                                    questionAnswerPair

                                Unanswered question ->
                                    Answered question answer
                        )
                    )
                |> updateAnswers
                    (List.Extra.remove answer)


scoreQuestion : Question -> Answer -> Score
scoreQuestion question answer =
    case question of
        Addition left right ->
            let
                correctAnswer =
                    left + right
            in
            if answer == correctAnswer then
                Perfect

            else if abs (answer - correctAnswer) <= config.prettyGoodMargin then
                PrettyGood

            else if abs (answer - correctAnswer) <= config.sureMargin then
                Sure

            else
                WhatTheHeck


percentTimeLeft : Game -> Float
percentTimeLeft { timeLeft } =
    timeLeft / config.roundDuration


type UpdateGameResult
    = GameEnded CompletedGame
    | GameContinues (Random.Generator Game)


updateAnswerTimeLeft : Float -> Game -> Random.Generator Game
updateAnswerTimeLeft delta game =
    if game.newAnswerTimeLeft - delta <= 0 then
        let
            { lowerBound, upperBound } =
                config.newAnswerTimeSlow
        in
        Random.map2
            (\newAnswerTimeLeft newAnswer ->
                { game | newAnswerTimeLeft = newAnswerTimeLeft, answers = List.append game.answers [ newAnswer ] }
            )
            (Random.float lowerBound upperBound)
            answerGenerator

    else
        Random.constant { game | newAnswerTimeLeft = game.newAnswerTimeLeft - delta }


handleAnimationFrameDelta : Float -> Game -> UpdateGameResult
handleAnimationFrameDelta delta game =
    if game.timeLeft - delta <= 0 then
        GameEnded (completeGame game)

    else
        game
            |> (\g -> { g | timeLeft = g.timeLeft - delta })
            |> updateAnswerTimeLeft delta
            |> GameContinues


completeGame : Game -> CompletedGame
completeGame game =
    { completedSheets = game.currentSheet :: game.completedSheets }


scoreToPoints : Score -> Int
scoreToPoints score =
    case score of
        Perfect ->
            config.points.perfect

        PrettyGood ->
            config.points.prettyGood

        Sure ->
            config.points.sure

        WhatTheHeck ->
            config.points.whatTheHeck


type alias ScoreRecord a =
    { perfect : a, prettyGood : a, sure : a, whatTheHeck : a }


getByScore : Score -> ScoreRecord a -> a
getByScore score =
    case score of
        Perfect ->
            .perfect

        PrettyGood ->
            .prettyGood

        Sure ->
            .sure

        WhatTheHeck ->
            .whatTheHeck


type alias GameSummary =
    { finalScore : Int, scoreCounts : ScoreRecord Int }


gameSummary : CompletedGame -> GameSummary
gameSummary { completedSheets } =
    let
        reduceFn : QuestionAnswerPair -> ScoreRecord Int -> ScoreRecord Int
        reduceFn questionAnswerPair accum =
            case questionAnswerPair of
                Unanswered _ ->
                    accum

                Answered question answer ->
                    case scoreQuestion question answer of
                        Perfect ->
                            { accum
                                | perfect = accum.perfect + 1
                            }

                        PrettyGood ->
                            { accum
                                | prettyGood = accum.prettyGood + 1
                            }

                        Sure ->
                            { accum
                                | perfect = accum.sure + 1
                            }

                        WhatTheHeck ->
                            { accum
                                | perfect = accum.whatTheHeck + 1
                            }

        scoreRecord : ScoreRecord Int
        scoreRecord =
            List.foldr reduceFn (ScoreRecord 0 0 0 0) (List.concat completedSheets)

        finalScore : Int
        finalScore =
            scoreToPoints Perfect
                * scoreRecord.perfect
                + scoreToPoints PrettyGood
                * scoreRecord.prettyGood
                + scoreToPoints Sure
                * scoreRecord.sure
                + scoreToPoints WhatTheHeck
                * scoreRecord.whatTheHeck
    in
    { scoreCounts = scoreRecord
    , finalScore = finalScore
    }


allQuestionsAnswered : Game -> Bool
allQuestionsAnswered { currentSheet } =
    List.all
        (\questionAnswerPair ->
            case questionAnswerPair of
                Answered _ _ ->
                    True

                _ ->
                    False
        )
        currentSheet


addAnswer : Game -> Random.Generator Game
addAnswer game =
    answerGenerator
        |> Random.map
            (\answer ->
                { game | answers = List.append game.answers [ answer ] }
            )
