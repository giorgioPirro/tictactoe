import Html exposing (Html, text)
import Html.Attributes exposing (..)

main =
  Html.program
    { init = (0, Cmd.none)
    , view = always (text "hello world")
    , update = \msg model -> (0, Cmd.none)
    , subscriptions = always Sub.none
    }
