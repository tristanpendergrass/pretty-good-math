module Game exposing (..)

import Config exposing (config)
import Html.Attributes exposing (download)
import List
import List.Extra
import Math
import Random



-- To use nonPrimes, for example, you can output it or work with it in your application logic.


type GameType
    = GameAddition
    | GameMultiplication


type alias Answer =
    Int


type QuestionAnswerPair
    = Answered Math.Question Answer
    | Unanswered Math.Question


type alias Sheet =
    List QuestionAnswerPair


type alias Game =
    { completedSheets : List Sheet
    , currentSheet : Sheet
    , answers : List Answer
    , timeLeft : Float
    , newAnswerTimeLeft : Float
    , gameType : GameType
    }


type alias CompletedGame =
    { completedSheets : List Sheet
    }


getStats : GameType -> Math.QuestionTypeStats
getStats gameType =
    case gameType of
        GameAddition ->
            Math.additionStats

        GameMultiplication ->
            Math.multiplicationStats



-- Generators


sheetGenerator : Random.Generator Math.Question -> Random.Generator Sheet
sheetGenerator qGenerator =
    Random.list config.questionsPerSheet (Random.map Unanswered qGenerator)


newGameGenerator : GameType -> Random.Generator Game
newGameGenerator gameType =
    let
        questionTypeStats : Math.QuestionTypeStats
        questionTypeStats =
            getStats gameType
    in
    Random.map
        (\initialSheet ->
            { completedSheets = []
            , currentSheet = initialSheet
            , answers = []
            , timeLeft = config.roundDuration

            -- For his first answer we don't make it random but rather make it happen right away to get the game going
            , newAnswerTimeLeft = config.newAnswerTimeFast.lowerBound
            , gameType = gameType
            }
        )
        (sheetGenerator questionTypeStats.questionGenerator)


goNextSheetGenerator : Game -> Random.Generator Game
goNextSheetGenerator game =
    let
        questionTypeStats : Math.QuestionTypeStats
        questionTypeStats =
            getStats game.gameType
    in
    Random.map
        (\newSheet ->
            { game
                | currentSheet = newSheet
                , answers = []
                , completedSheets = game.currentSheet :: game.completedSheets
                , newAnswerTimeLeft = config.newAnswerTimeFast.lowerBound
            }
        )
        (sheetGenerator questionTypeStats.questionGenerator)



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
                |> (\g -> { g | newAnswerTimeLeft = config.newAnswerTimeFast.lowerBound })


percentTimeLeft : Game -> Float
percentTimeLeft { timeLeft } =
    timeLeft / config.roundDuration


type UpdateGameResult
    = GameEnded CompletedGame
    | GameContinues (Random.Generator Game)


updateAnswerTimeLeft : Float -> Game -> Random.Generator Game
updateAnswerTimeLeft delta game =
    if List.length game.answers >= config.maxAnswers then
        Random.constant game

    else if game.newAnswerTimeLeft - delta <= 0 then
        let
            { lowerBound, upperBound } =
                config.newAnswerTimeSlow

            questionTypeStats =
                getStats game.gameType
        in
        Random.map2
            (\newAnswerTimeLeft newAnswer ->
                { game | newAnswerTimeLeft = newAnswerTimeLeft, answers = List.append game.answers [ newAnswer ] }
            )
            (Random.float lowerBound upperBound)
            questionTypeStats.answerGenerator

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


scoreToPoints : Math.Score -> Int
scoreToPoints score =
    case score of
        Math.Perfect ->
            config.points.perfect

        Math.PrettyGood ->
            config.points.prettyGood

        Math.Sure ->
            config.points.sure

        Math.WhatTheHeck ->
            config.points.whatTheHeck


type alias ScoreRecord a =
    { perfect : a, prettyGood : a, sure : a, whatTheHeck : a }


getByScore : Math.Score -> ScoreRecord a -> a
getByScore score =
    case score of
        Math.Perfect ->
            .perfect

        Math.PrettyGood ->
            .prettyGood

        Math.Sure ->
            .sure

        Math.WhatTheHeck ->
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
                    case Math.scoreQuestion question answer of
                        Math.Perfect ->
                            { accum
                                | perfect = accum.perfect + 1
                            }

                        Math.PrettyGood ->
                            { accum
                                | prettyGood = accum.prettyGood + 1
                            }

                        Math.Sure ->
                            { accum
                                | perfect = accum.sure + 1
                            }

                        Math.WhatTheHeck ->
                            { accum
                                | perfect = accum.whatTheHeck + 1
                            }

        scoreRecord : ScoreRecord Int
        scoreRecord =
            List.foldr reduceFn (ScoreRecord 0 0 0 0) (List.concat completedSheets)

        finalScore : Int
        finalScore =
            scoreToPoints Math.Perfect
                * scoreRecord.perfect
                + scoreToPoints Math.PrettyGood
                * scoreRecord.prettyGood
                + scoreToPoints Math.Sure
                * scoreRecord.sure
                + scoreToPoints Math.WhatTheHeck
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
    let
        questionTypeStats : Math.QuestionTypeStats
        questionTypeStats =
            getStats game.gameType
    in
    Random.map
        (\answer ->
            { game | answers = List.append game.answers [ answer ] }
        )
        questionTypeStats.answerGenerator
