module Main exposing (..)

import Browser
import Html exposing (Html, button, code, div, img, p, pre, text, textarea)
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
    | CodeBlock String String



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
        , body = Http.jsonBody (messagesEncoder messages)
        , expect = Http.expectJson ReceiveMessages messagesDecoder
        }



-- Encoders


messagesEncoder : List Message -> Encode.Value
messagesEncoder messages =
    Encode.object [ ( "messages", Encode.list messageEncoder messages ) ]


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



-- Creates a div that will contain everything that belongs to a single
-- conversational element, e.g. a single complete response from the AI


viewMessage message =
    let
        -- Split the message to multiple parts for easier presentation
        contents =
            splitContent message.text

        -- Get the images
        images =
            message.images

        -- Get the image placeholders indexed for easier matching.
        augmentedContents =
            augmentContents 0 contents

        roleClass =
            "message-container-" ++ message.role
    in
    div [ class "message-container", class roleClass ] (viewContents images augmentedContents)



-- Renders a list of divs within a single conversational element.


viewContents : List Image -> List ( Content, Maybe Int ) -> List (Html msg)
viewContents images augmentedContents =
    let
        helper augmentedContent =
            case augmentedContent of
                ( Text txt, Nothing ) ->
                    div [ class "text-content" ] [ p [] [ text txt ] ]

                ( ImagePlaceholder alttext, Just index ) ->
                    case List.drop index images of
                        image :: rest ->
                            div [ class "image-content" ] [ img [ src image.url, alt alttext, class "message-image" ] [] ]

                        [] ->
                            div [] []

                ( CodeBlock lang txt, Nothing ) ->
                    div [ class "code-content" ] [ pre [] [ code [] [ text txt ] ] ]

                _ ->
                    div [] []
    in
    List.map helper augmentedContents



-- Helpers
-- Count occurences of substring in a string


countSubString : String -> String -> Int
countSubString substr str =
    String.split substr str
        |> List.length
        |> (\x -> x - 1)



-- Check that there is a equal amount of openings and endings


assertMatchingPairs : String -> String -> String -> Bool
assertMatchingPairs text left right =
    countSubString left text == countSubString right text



-- Check that there is an even amount of occurences.


assertEven : String -> String -> Bool
assertEven substr str =
    modBy 2 (countSubString substr str) == 0



-- Pairs each image placeholder with a index


augmentContents : Int -> List Content -> List ( Content, Maybe Int )
augmentContents index contents =
    case contents of
        [] ->
            []

        content :: rest ->
            case content of
                ImagePlaceholder inner ->
                    ( content, Just index ) :: augmentContents (index + 1) rest

                _ ->
                    ( content, Nothing ) :: augmentContents index rest



-- A hard-working function that finds patterns inside a big string by dividing
-- it into a "foreground" and "background" parts one a at a time.


splitContentHelper : (String -> Content) -> String -> String -> String -> List Content
splitContentHelper constructor left right remainingText =
    if String.isEmpty remainingText then
        []

    else
        -- Are we at the beginning of the "foreground" part or not?
        case String.startsWith left remainingText of
            -- If yes, take the contents within the wrapping
            -- markings and call itself with the text after
            -- the closing markings.
            True ->
                let
                    rest =
                        String.dropLeft (String.length left) remainingText

                    substrEndIndex =
                        String.indexes right rest
                            |> List.head
                            |> Maybe.withDefault (String.length rest)

                    obj =
                        String.slice 0 substrEndIndex rest
                            |> String.trim
                            |> constructor

                    remaining =
                        String.dropLeft (substrEndIndex + String.length right) rest
                in
                obj :: splitContentHelper constructor left right remaining

            -- If not, either find the next opening marking, or, if not present,
            -- the end of the string, and then take the "background" text and
            -- call itself either with the empty string, or from the start of the next
            -- "foreground" marking.
            False ->
                let
                    nextSubstrIndex =
                        String.indexes left remainingText
                            |> List.head
                            |> Maybe.withDefault (String.length remainingText)

                    textContent =
                        String.slice 0 nextSubstrIndex remainingText
                            |> String.trim
                            |> Text

                    remaining =
                        String.dropLeft nextSubstrIndex remainingText
                in
                textContent :: splitContentHelper constructor left right remaining



-- Takes a conversational element in string and splits it into
-- interesting subparts that are easy to render.


splitContent : String -> List Content
splitContent fullText =
    let
        -- Helper to split a big text to its image and text parts.
        imagesSplit text =
            if assertMatchingPairs "{{" "}}" fullText then
                splitContentHelper ImagePlaceholder "{{" "}}" text

            else
                []

        -- Helper to create a Code Block element from
        -- "python\n print("Hello world")"
        -- type of string.
        codeBlockFromText codeText =
            let
                parts =
                    String.split "\n" codeText

                lang =
                    List.head parts
                        |> Maybe.withDefault ""

                code =
                    List.tail parts
                        |> Maybe.withDefault []
                        |> String.join "\n"
            in
            CodeBlock lang code

        -- Helper that takes a Content object and if it is of Text type,
        -- splits it into normal text and code blocks.
        codeBlockSplit content =
            case content of
                Text str ->
                    if assertEven "```" fullText then
                        splitContentHelper codeBlockFromText "```" "```" str

                    else
                        [ content ]

                _ ->
                    [ content ]

        -- Helper that takes a Content object and if it is of Text type,
        -- splits it by linebreaks
        lineBreakSplit content =
            case content of
                Text str ->
                    String.split "\n" str
                        |> List.map Text

                _ ->
                    [ content ]
    in
    fullText
        |> imagesSplit
        |> List.concat
        << List.map codeBlockSplit
        |> List.concat
        << List.map lineBreakSplit
