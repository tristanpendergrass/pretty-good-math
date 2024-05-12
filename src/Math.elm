module Math exposing (..)

import Config exposing (..)
import Random


type alias Answer =
    Int


type Question
    = Addition Int Int
    | AdditionBig Int Int
    | Multiplication Int Int


type Score
    = Perfect
    | PrettyGood
    | Sure
    | WhatTheHeck


type alias QuestionTypeStats =
    { title : String

    -- marginLabels is how close the answer has to be. String will be shown directly to the user in UI.
    , margins : { prettyGood : Int, sure : Int }
    , answerGenerator : Random.Generator Answer
    , questionGenerator : Random.Generator Question
    }


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

            else if abs (answer - correctAnswer) <= additionStats.margins.prettyGood then
                PrettyGood

            else if abs (answer - correctAnswer) <= additionStats.margins.sure then
                Sure

            else
                WhatTheHeck

        AdditionBig left right ->
            let
                correctAnswer =
                    left + right
            in
            if answer == correctAnswer then
                Perfect

            else if abs (answer - correctAnswer) <= additionBigStats.margins.prettyGood then
                PrettyGood

            else if abs (answer - correctAnswer) <= additionBigStats.margins.sure then
                Sure

            else
                WhatTheHeck

        Multiplication left right ->
            let
                correctAnswer =
                    left * right
            in
            if answer == correctAnswer then
                Perfect

            else if abs (answer - correctAnswer) <= multiplicationStats.margins.prettyGood then
                PrettyGood

            else if abs (answer - correctAnswer) <= multiplicationStats.margins.sure then
                Sure

            else
                WhatTheHeck



-- Addition


additionStats : QuestionTypeStats
additionStats =
    { title = "Addition"
    , margins =
        { prettyGood = config.additionAnswerMargins.prettyGood
        , sure = config.additionAnswerMargins.sure
        }
    , answerGenerator =
        Random.int
            (config.addendLowerBound * 2)
            (config.addendUpperBound * 2)
    , questionGenerator =
        let
            addendGenerator : Random.Generator Int
            addendGenerator =
                Random.int config.addendLowerBound config.addendUpperBound
        in
        Random.map2 Addition addendGenerator addendGenerator
    }



-- Addition Big


additionBigStats : QuestionTypeStats
additionBigStats =
    { title = "Big Addition"
    , margins =
        { prettyGood = config.additionBigAnswerMargins.prettyGood
        , sure = config.additionBigAnswerMargins.sure
        }
    , answerGenerator =
        Random.int
            (config.addendBigLowerBound * 2)
            (config.addendBigUpperBound * 2)
    , questionGenerator =
        let
            addendGenerator : Random.Generator Int
            addendGenerator =
                Random.int config.addendBigLowerBound config.addendBigUpperBound
        in
        Random.map2 Addition addendGenerator addendGenerator
    }



-- Multiplication


multiplicationAnswers : ( Int, List Int )
multiplicationAnswers =
    let
        reduceFn : Int -> List Int -> List Int
        reduceFn m accum =
            List.range m config.multiplicandUpperBound
                |> List.map ((*) m)
                |> List.append accum
    in
    List.range config.multiplicandLowerBound config.multiplicandUpperBound
        |> List.foldl reduceFn []
        |> (\res ->
                let
                    rest : List Int
                    rest =
                        List.drop 1 res

                    first : Int
                    first =
                        config.multiplicandLowerBound * config.multiplicandLowerBound
                in
                ( first, rest )
           )


multiplicationStats : QuestionTypeStats
multiplicationStats =
    { title = "Multiplication"
    , margins =
        { prettyGood = config.multiplicationAnswerMargins.prettyGood
        , sure = config.multiplicationAnswerMargins.sure
        }
    , answerGenerator =
        let
            ( firstAnswer, restAnswers ) =
                multiplicationAnswers
        in
        Random.uniform firstAnswer restAnswers
    , questionGenerator =
        let
            multiplicandGenerator : Random.Generator Int
            multiplicandGenerator =
                Random.int config.multiplicandLowerBound config.multiplicandUpperBound
        in
        Random.map2 Multiplication multiplicandGenerator multiplicandGenerator
    }
