module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, text, p, textarea)
import Html.Attributes exposing (alt, class, id, placeholder, rows, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import String



-- Main


main : Program Decode.Value Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- Aliases


type alias Image =
    { url : String
    }


type alias Message =
    { role : String
    , text : String
    , images : List Image
    }


type alias Model =
    { messages : List Message
    , input : String
    , loading : Bool
    , apiAddress : String
    }



-- Types


type Content
    = Text String
    | ImagePlaceholder String



-- Init


init : Decode.Value -> ( Model, Cmd Msg )
init flagsMsg =
    let
        apiAddress =
            Decode.decodeValue apiAddressDecoder flagsMsg |> Result.withDefault "http://localhost:8000"
    in
    ( { messages = [], input = "", loading = False, apiAddress = apiAddress }
    , Cmd.none
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


type Msg
    = NewMessage String
    | SendMessage String
    | ReceiveMessages (Result Http.Error (List Message))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMessage content ->
            ( { model | input = content }, Cmd.none )

        SendMessage content ->
            let
                newMessages =
                    model.messages ++ [ { role = "user", text = content, images = [] } ]
            in
            ( { model | messages = newMessages, loading = True, input = "" }
            , postMessages newMessages model.apiAddress
            )

        ReceiveMessages result ->
            case result of
                Ok newMessages ->
                    ( { model | messages = newMessages, loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )



-- Commands


postMessages : List Message -> String -> Cmd Msg
postMessages messages apiAddress =
    Http.post
        { url = apiAddress ++ "/generate_response"
        , body = Http.jsonBody (Encode.object [ ( "messages", Encode.list messageEncoder messages ) ])
        , expect = Http.expectJson ReceiveMessages messagesDecoder
        }



-- Encoders


messageEncoder : Message -> Encode.Value
messageEncoder message =
    Encode.object
        [ ( "role", Encode.string message.role )
        , ( "text", Encode.string message.text )
        , ( "images", Encode.list imageEncoder message.images )
        ]


imageEncoder : Image -> Encode.Value
imageEncoder image =
    Encode.object
        [ ( "url", Encode.string image.url )
        ]



-- Decoders


apiAddressDecoder : Decoder String
apiAddressDecoder =
    Decode.field "apiAddress" Decode.string


messagesDecoder : Decoder (List Message)
messagesDecoder =
    Decode.field "messages" (Decode.list messageDecoder)


messageDecoder : Decoder Message
messageDecoder =
    Decode.map3 Message
        (Decode.field "role" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "images" (Decode.list imageDecoder))


imageDecoder : Decoder Image
imageDecoder =
    Decode.map Image
        (Decode.field "url" Decode.string)



-- View


view : Model -> Html Msg
view model =
    let
        messages =
            model.messages

        -- debugmessage = Debug.log "messages" messages
    in
    div []
        [ div [ class "messages-container" ] (List.map viewMessage messages)
        , if model.loading then
            div [ class "input-container-loading" ] [ text "Loading..." ]

          else
            div [ class "input-container" ]
                [ button [ onClick (SendMessage model.input) ] [ text "Send" ]
                , textarea [ placeholder "Type your message", onInput NewMessage, value model.input, rows 1 ] []
                ]
        ]


viewMessage message =
    let
        contents =
            splitContent message.text

        images =
            message.images

        roleClass =
            "message-container-" ++ message.role

        augmentedContents =
            augmentContents 0 contents
    in
    div [ class "message-container", class roleClass ] (viewContents images augmentedContents)


viewContents : List Image -> List ( Content, Maybe Int ) -> List (Html msg)
viewContents images augmentedContents =
    let
        helper augmentedContent =
            case augmentedContent of
                ( Text txt, Nothing ) ->
                    div [ class "text-content" ] [ p [] [text txt] ]

                ( ImagePlaceholder alttext, Just index ) ->
                    case List.drop index images of
                        image :: rest ->
                            div [ class "image-content" ] [ img [ src image.url, alt alttext, class "message-image" ] [] ]

                        [] ->
                            div [] []

                _ ->
                    div [] []
    in
    List.map helper augmentedContents



-- Helpers


augmentContents : Int -> List Content -> List ( Content, Maybe Int )
augmentContents index contents =
    case contents of
        [] ->
            []

        content :: rest ->
            case content of
                Text inner ->
                    ( content, Nothing ) :: augmentContents index rest

                ImagePlaceholder inner ->
                    ( content, Just index ) :: augmentContents (index + 1) rest


indexes : String -> String -> List Int
indexes needle haystack =
    let
        helper idx str acc =
            case String.indexes needle str of
                [] ->
                    List.reverse acc

                h :: _ ->
                    let
                        newIndex =
                            idx + h
                    in
                    helper (newIndex + String.length needle) (String.dropLeft (h + String.length needle) str) (newIndex :: acc)
    in
    helper 0 haystack []


countSubString : String -> String -> Int
countSubString subStr str =
    String.split subStr str
        |> List.length
        |> (\x -> x - 1)


countTags : String -> ( Int, Int )
countTags text =
    ( countSubString "{{" text, countSubString "}}" text )


splitContentHelper : String -> List Content
splitContentHelper remainingText =
    if String.isEmpty remainingText then
        []

    else
        case String.startsWith "{{" remainingText of
            True ->
                let
                    imageTagEndIndex =
                        indexes "}}" remainingText
                            |> List.head
                            |> Maybe.withDefault (String.length remainingText)

                    imagePlaceholder =
                        String.slice 0 imageTagEndIndex remainingText
                            |> String.dropLeft 2
                            |> String.trim
                            |> ImagePlaceholder

                    remaining =
                        String.dropLeft (imageTagEndIndex + 2) remainingText
                in
                imagePlaceholder :: splitContentHelper remaining

            False ->
                let
                    nextImageTagIndex =
                        indexes "{{" remainingText
                            |> List.head
                            |> Maybe.withDefault (String.length remainingText)

                    textContent =
                        String.slice 0 nextImageTagIndex remainingText
                            |> String.trim
                            |> Text

                    remaining =
                        String.dropLeft nextImageTagIndex remainingText
                in
                textContent :: splitContentHelper remaining


splitContent : String -> List Content
splitContent text =
    let
        ( startTags, endTags ) =
            countTags text
    in
    if startTags /= endTags then
        []

    else
        splitContentHelper text
