module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Browser
import Dict
import Html exposing (Html, a, aside, div, li, section, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Parts.DynamicDownloader as Downloader
import Parts.FiletableSortableTable as FSTable



---- MODEL ----


type alias Model =
    { parts : Maybe (Html Msg)
    , fstable : FSTable.Model FSTable.Msg
    , downloader : Downloader.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { parts = Nothing, fstable = FSTable.init, downloader = Downloader.init }, Cmd.none )


initItems =
    { heads =
        [ { label = "ID", filtable = False, sortable = True }
        , { label = "名前", filtable = True, sortable = True }
        , { label = "値段", filtable = False, sortable = True }
        , { label = "割引率", filtable = False, sortable = True }
        , { label = "備考", filtable = True, sortable = False }
        ]
    , lines =
        [ [ FSTable.Num 1, FSTable.Str "みかん", FSTable.Str "時価", FSTable.View { value = FSTable.Num 0, view = div [] [ text "-" ] }, FSTable.Str "おいしい" ]
        , [ FSTable.Num 2, FSTable.Str "りんご", FSTable.Num 80, FSTable.View { value = FSTable.Num 5, view = div [] [ div [] [ text "5%" ], div [] [ text "(6月まで)" ] ] }, FSTable.Str "とてもおいしい" ]
        , [ FSTable.Num 3, FSTable.Str "バナナ", FSTable.Num 200, FSTable.View { value = FSTable.Num 10, view = div [] [ text "10%" ] }, FSTable.Str "すぐおいしい" ]
        , [ FSTable.Num 4, FSTable.Str "スイカ", FSTable.Num 300, FSTable.View { value = FSTable.Num 0, view = div [] [ text "(7月から)" ] }, FSTable.Str "すごくおいしい" ]
        ]
            |> List.map (\line -> { items = Array.fromList line, viewable = True })
    , filterConditions = Dict.empty
    , sortCondition = Nothing
    }



---- UPDATE ----


type Msg
    = NoOp
    | ShowFSTable
    | ShowDownloader
    | FSTableMsg FSTable.Msg
    | DownloaderMsg Downloader.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowFSTable ->
            ( { model | parts = Just (Html.map FSTableMsg (FSTable.view initItems)), fstable = initItems }, Cmd.none )

        ShowDownloader ->
            ( { model | parts = Just (Html.map DownloaderMsg (Downloader.view Downloader.init)), downloader = Downloader.init }, Cmd.none )

        FSTableMsg subMsg ->
            let
                newItems =
                    FSTable.update subMsg model.fstable
            in
            ( { model | parts = Just (Html.map FSTableMsg (FSTable.view newItems)), fstable = newItems }, Cmd.none )

        DownloaderMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Downloader.update subMsg model.downloader
            in
            ( { model | parts = Just (Html.map DownloaderMsg (Downloader.view newModel)), downloader = newModel }, cmd )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        parts =
            (model.parts |> Maybe.map List.singleton) |> Maybe.withDefault []
    in
    section [ class "section" ]
        [ div [ class "columns" ]
            [ div [ class "column is-one-fifth" ]
                [ aside [ class "menu" ]
                    [ ul [ class "menu-list" ]
                        [ li [ onClick ShowFSTable ] [ a [] [ text "フィルタ・ソート機能付きテーブル" ] ]
                        , li [ onClick ShowDownloader ] [ a [] [ text "動的ダウンローダー" ] ]
                        ]
                    ]
                ]
            , div [ class "container column" ] parts
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
