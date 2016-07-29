module Main exposing (..)

import Html.App as App
import Weather exposing (init, view, update)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
