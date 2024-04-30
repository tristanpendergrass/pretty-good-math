module Config exposing (config)


config : Config
config =
    -- prodConfig
    devConfig


type alias Config =
    { roundDuration : Float -- Duration in milliseconds
    , addendLowerBound : Int -- Bounds for generating addition problems
    , addendUpperBound : Int
    , prettyGoodMargin : Int -- "Pretty Good" if abs(actual - given) answer is equal to or less than this number
    , sureMargin : Int -- "Sure" if abs(actual - given) answer is equal to or less than this number
    , questionsPerSheet : Int -- How many questions shown on a single sheet
    , answersPerSheet : Int -- How many answers to generate when showing a new sheet
    , points : { perfect : Int, prettyGood : Int, sure : Int, whatTheHeck : Int }
    , passPoints : Int
    }


prodConfig : Config
prodConfig =
    { roundDuration = 30 * 1000
    , addendLowerBound = 1
    , addendUpperBound = 10
    , prettyGoodMargin = 2
    , sureMargin = 4
    , questionsPerSheet = 5
    , answersPerSheet = 5
    , points = { perfect = 3, prettyGood = 2, sure = 1, whatTheHeck = 0 }
    , passPoints = 20
    }


devConfig : Config
devConfig =
    { prodConfig
        | roundDuration = 1000 * 1000
    }
