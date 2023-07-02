module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Main


main : Program Json.Decode.Value Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- Model


type alias Model =
    { messages : List String
    , input : String
    , loading : Bool
    }


-- Init


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flagsMsg =
    ( { messages = [], input = "", loading = False }
    , Cmd.none
    )


-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none


-- Update


type Msg
    = NewMessage String
    | SendMessage
    | ReceiveMessages (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMessage content ->
            ( { model | input = content }, Cmd.none )

        SendMessage ->
            ( { model | loading = True }
            , postMessages model.messages
            )

        ReceiveMessages result ->
            case result of
                Ok newMessages ->
                    ( { model | messages = newMessages, loading = False, input = "" }, Cmd.none )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )



-- Commands


postMessages : List String -> Cmd Msg
postMessages messages =
    Http.post
        { url = "https://imagellm.teekuningas.net/api"
        , body = Http.jsonBody (Encode.list Encode.string messages)
        , expect = Http.expectJson ReceiveMessages messagesDecoder
        }



-- Decoders


messagesDecoder : Decoder (List String)
messagesDecoder =
    Decode.field "messages" (Decode.list Decode.string)



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMessage model.messages)
        , input [ placeholder "Type your message", onInput NewMessage ] []
        , button [ onClick SendMessage ] [ text "Send" ]
        ]


viewMessage : String -> Html msg
viewMessage message =
    div [] [ text message ]
