port module Main exposing (main)

import Platform exposing (Program)
import Time



-- PORTS


port fromJavascript : (String -> msg) -> Sub msg


port toJavascript : String -> Cmd msg



-- MODEL


type alias Model =
    String



-- MSG


type Msg
    = FromJavascript String
    | Tick Time.Posix



-- MAIN


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( "init", Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromJavascript str ->
            ( str, toJavascript ("I received a message, changing the model to: " ++ str) )

        Tick time ->
            ( model, toJavascript ("Tick " ++ (time |> Time.posixToMillis |> String.fromInt) ++ "; Model " ++ model) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fromJavascript FromJavascript
        , Time.every 1000 Tick
        ]
