module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onInput)
import List exposing (foldl, map)
import String exposing (toList)


main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { floor : Int }


type Message
    = DeliverPresents String


init : () -> ( Model, Cmd Message )
init () =
    ( { floor = 0 }, Cmd.none )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( case message of
        DeliverPresents puzzle ->
            { floor = floorOf puzzle }
    , Cmd.none
    )


view : Model -> Browser.Document Message
view model =
    { title = "Day 1: Not Quite Lisp"
    , body =
        [ div []
            [ input [ type_ "text", placeholder "Puzzle", onInput DeliverPresents ]
                []
            , text (String.fromInt model.floor)
            ]
        ]
    }


floorOf : String -> Int
floorOf puzzle =
    foldl (+)
        0
        (map
            (\p ->
                case p of
                    '(' ->
                        1

                    ')' ->
                        -1

                    _ ->
                        0
            )
            (toList puzzle)
        )
