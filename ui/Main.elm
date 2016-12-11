module Main exposing (main)

import Html as Html
import Scene exposing (Model, Msg, init, update, view, subscriptions)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
