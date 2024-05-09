module Config exposing (config)


config : Config
config =
    -- prodConfig
    devConfig


type alias Config =
    { roundDuration : Float -- Duration in milliseconds
    , addendLowerBound : Int -- Bounds for generating addition problems
    , addendUpperBound : Int
    , multiplicandLowerBound : Int -- Bounds for generating multiplication problems
    , multiplicandUpperBound : Int
    , prettyGoodMargin : Int -- "Pretty Good" if abs(actual - given) answer is equal to or less than this number
    , sureMargin : Int -- "Sure" if abs(actual - given) answer is equal to or less than this number
    , questionsPerSheet : Int -- How many questions shown on a single sheet
    , answersPerSheet : Int -- How many answers to generate when showing a new sheet
    , points : { perfect : Int, prettyGood : Int, sure : Int, whatTheHeck : Int }
    , passPoints : Int
    , newAnswerTimeSlow : { lowerBound : Float, upperBound : Float }
    , newAnswerTimeFast : { lowerBound : Float, upperBound : Float }
    , maxAnswers : Int
    }


prodConfig : Config
prodConfig =
    { roundDuration = 60 * 1000
    , addendLowerBound = 1
    , addendUpperBound = 10
    , multiplicandLowerBound = 2
    , multiplicandUpperBound = 8
    , prettyGoodMargin = 2
    , sureMargin = 4
    , questionsPerSheet = 5
    , answersPerSheet = 5
    , points = { perfect = 3, prettyGood = 2, sure = 1, whatTheHeck = 0 }
    , passPoints = 20
    , newAnswerTimeSlow = { lowerBound = 2000, upperBound = 3500 }
    , newAnswerTimeFast = { lowerBound = 250, upperBound = 1000 }
    , maxAnswers = 10
    }


devConfig : Config
devConfig =
    { prodConfig
        | roundDuration = 1000 * 1000
        , answersPerSheet = 8
    }
