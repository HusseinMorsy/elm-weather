module Main exposing (..)

import ElmTest exposing (..)
import Weather exposing (City, update)


tests : ElmTest.Test
tests =
    let
        dus =
            City 1 "Düsseldorf" (Just 20) Weather.Completed

        muc =
            City 2 "Munich" (Just 22) Weather.Completed

        cities =
            [ dus, muc ]

        model =
            { cities = cities, nameInput = "", nextId = 3 }
    in
        suite "update"
            [ test "NoOp" <|
                assertEqual
                    (( model, Cmd.none ))
                    (update Weather.NoOp model)
            , test "UpdateNameField" <|
                assertEqual
                    ({ model | nameInput = "test" } ! [])
                    (update (Weather.UpdateNameField "test") model)
            ]


main : Program Never
main =
    runSuite tests
