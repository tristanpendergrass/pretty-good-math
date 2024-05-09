module Math exposing (..)

import Config exposing (..)
import Random


type alias Answer =
    Int


type Question
    = Addition Int Int
    | Multiplication Int Int


type Score
    = Perfect
    | PrettyGood
    | Sure
    | WhatTheHeck


type alias QuestionTypeStats =
    { title : String
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

            else if abs (answer - correctAnswer) <= config.prettyGoodMargin then
                PrettyGood

            else if abs (answer - correctAnswer) <= config.sureMargin then
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

            else if abs (answer - correctAnswer) <= config.prettyGoodMargin then
                PrettyGood

            else if abs (answer - correctAnswer) <= config.sureMargin then
                Sure

            else
                WhatTheHeck



-- Addition


additionStats : QuestionTypeStats
additionStats =
    { title = "Addition"
    , answerGenerator =
        Random.int
            (config.multiplicandLowerBound + config.multiplicandLowerBound)
            (config.multiplicandUpperBound + config.multiplicandUpperBound)
    , questionGenerator =
        let
            addendGenerator : Random.Generator Int
            addendGenerator =
                Random.int config.multiplicandLowerBound config.multiplicandUpperBound
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
