module Config exposing (config)


config : Config
config =
    -- prodConfig
    devConfig


type alias Config =
    { roundDuration : Float -- Duration in milliseconds
    , addendLowerBound : Int -- Bounds for generating addition problems
    , addendUpperBound : Int
    , addendBigLowerBound : Int -- Bounds for generating addition problems
    , addendBigUpperBound : Int
    , multiplicandLowerBound : Int -- Bounds for generating simple multiplication problems
    , multiplicandUpperBound : Int -- Bounds for generating simple multiplication problems

    -- Unlike the regular multiplication, we have two separate bounds for the two multiplicands with big multiplication to make it more interesting
    , multiplicandBigLowerLowerBound : Int -- Bounds for generating the left side of x * y in big multiplication
    , multiplicandBigLowerUpperBound : Int
    , multiplicandBigUpperLowerBound : Int -- Bounds for generating the right side of x * y in big multiplication
    , multiplicandBigUpperUpperBound : Int
    , additionAnswerMargins : { prettyGood : Int, sure : Int }
    , additionBigAnswerMargins : { prettyGood : Int, sure : Int }
    , multiplicationAnswerMargins : { prettyGood : Int, sure : Int }
    , multiplicationBigAnswerMargins : { prettyGood : Int, sure : Int }
    , questionsPerSheet : Int -- How many questions shown on a single sheet
    , answersPerSheet : Int -- How many answers to generate when showing a new sheet
    , points : { perfect : Int, prettyGood : Int, sure : Int, whatTheHeck : Int }
    , newAnswerTimeSlow : { lowerBound : Float, upperBound : Float }
    , newAnswerTimeFast : { lowerBound : Float, upperBound : Float }
    , maxAnswers : Int
    , grades :
        { a : Int
        , b : Int
        , c : Int
        , d : Int
        }
    }


prodConfig : Config
prodConfig =
    { roundDuration = 60 * 1000
    , addendLowerBound = 1
    , addendUpperBound = 10
    , addendBigLowerBound = 10
    , addendBigUpperBound = 499
    , multiplicandLowerBound = 2
    , multiplicandUpperBound = 8
    , multiplicandBigLowerLowerBound = 2
    , multiplicandBigLowerUpperBound = 12
    , multiplicandBigUpperLowerBound = 21
    , multiplicandBigUpperUpperBound = 49
    , additionAnswerMargins = { prettyGood = 2, sure = 4 }
    , additionBigAnswerMargins = { prettyGood = 80, sure = 160 }
    , multiplicationAnswerMargins = { prettyGood = 2, sure = 4 }
    , multiplicationBigAnswerMargins = { prettyGood = 40, sure = 80 }
    , questionsPerSheet = 5
    , answersPerSheet = 5
    , points = { perfect = 3, prettyGood = 2, sure = 1, whatTheHeck = 0 }
    , newAnswerTimeSlow = { lowerBound = 2000, upperBound = 3500 }
    , newAnswerTimeFast = { lowerBound = 250, upperBound = 1000 }
    , maxAnswers = 10
    , grades =
        { a = 20
        , b = 15
        , c = 10
        , d = 5
        }
    }


devConfig : Config
devConfig =
    { prodConfig
        | roundDuration = 1000 * 1000
        , answersPerSheet = 8
    }
