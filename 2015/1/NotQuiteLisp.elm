module NotQuiteLisp exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events
import List
import String


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
        [ Html.div []
            [ Html.input [ Html.Attributes.type_ "text", Html.Attributes.placeholder "Puzzle", Html.Events.onInput DeliverPresents ]
                []
            , Html.text (String.fromInt model.floor)
            ]
        ]
    }


floorOf : String -> Int
floorOf puzzle =
    List.foldl (+)
        0
        (List.map
            (\p ->
                case p of
                    '(' ->
                        1

                    ')' ->
                        -1

                    _ ->
                        0
            )
            (String.toList puzzle)
        )
