module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Browser
import Dict
import Html exposing (Html, a, aside, div, li, section, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Parts.FiletableSortableTable as FSTable



---- MODEL ----


type alias Model =
    { parts : Maybe (Html Msg)
    , items : FSTable.Model FSTable.Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { parts = Nothing, items = initItems }, Cmd.none )


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
    | ShowParts
    | FSTableMsg FSTable.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowParts ->
            ( { model | parts = Just (Html.map FSTableMsg (FSTable.view model.items)) }, Cmd.none )

        FSTableMsg subMsg ->
            let
                newItems =
                    FSTable.update subMsg model.items
            in
            ( { model | parts = Just (Html.map FSTableMsg (FSTable.view newItems)), items = newItems }, Cmd.none )



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
                        [ li [ onClick ShowParts ] [ a [] [ text "フィルタ・ソート機能付きテーブル" ] ]
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
