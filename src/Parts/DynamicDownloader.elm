module Parts.DynamicDownloader exposing (Model, Msg(..), init, update, view)

import Events exposing (onChange)
import Html exposing (Html, a, button, div, input, option, p, select, span, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (class, disabled, download, href, placeholder, rows, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Json
import Url exposing (percentEncode)



---- MODEL ----


type alias Model =
    { filename : String
    , contentType : String
    , contents : String
    , enableDownload : Bool
    }


init : Model
init =
    { filename = "", contentType = "text/plain", contents = "", enableDownload = False }



---- UPDATE ----


type Msg
    = InputFilename String
    | ChangeContentType String
    | InputContents String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        InputFilename filename ->
            ( { model | filename = filename, enableDownload = verify filename model.contentType model.contents }, Cmd.none )

        ChangeContentType contentType ->
            ( { model | contentType = contentType, enableDownload = verify model.filename contentType model.contents }, Cmd.none )

        InputContents contents ->
            ( { model | contents = contents, enableDownload = verify model.filename model.contentType contents }, Cmd.none )


verify : String -> String -> String -> Bool
verify filename contentType contents =
    String.isEmpty filename
        == False
        && String.isEmpty contents
        == False
        && (contentType /= "application/json" || verifyJson contents)


verifyJson : String -> Bool
verifyJson value =
    case Json.decodeString Json.value value of
        Ok _ ->
            True

        Err _ ->
            False



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        downloadHref =
            "data:" ++ model.contentType ++ "," ++ percentEncode model.contents

        downloadButton =
            button [ class "button", type_ "button", model.enableDownload == False |> disabled ] [ text "ダウンロード" ]
    in
    div [ class "box" ]
        [ div [ class "level" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ p [ class "control" ]
                        [ input [ class "input", type_ "text", placeholder "file name", onInput InputFilename ] []
                        ]
                    , p [ class "control" ]
                        [ span [ class "select" ]
                            [ select [ onChange ChangeContentType ]
                                [ option [ value "text/plain" ] [ text "プレーンテキスト" ]
                                , option [ value "text/csv" ] [ text "CSV" ]
                                , option [ value "application/json" ] [ text "JSON" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [] [ textarea [ class "textarea", rows 10, placeholder "contents", onInput InputContents ] [] ]
        , div []
            [ if model.enableDownload then
                a [ download model.filename, href downloadHref ] [ downloadButton ]

              else
                downloadButton
            ]
        ]
