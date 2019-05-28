module Parts.FiletableSortableTable exposing (Item(..), Model, Msg(..), update, view)

import Array
import Dict
import Html exposing (Html, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model msg =
    { heads : List Head
    , lines : List (Line msg)
    , filterConditions : Dict.Dict Int String
    , sortCondition : Maybe ( Int, Order )
    }


type alias Head =
    { label : String
    , filtable : Bool
    , sortable : Bool
    }


type alias ItemView msg =
    { value : Item msg
    , view : Html msg
    }


type Item msg
    = Str String
    | Num Int
    | View (ItemView msg)


type alias Line msg =
    { items : Array.Array (Item msg)
    , viewable : Bool
    }


type Order
    = Asc
    | Desc



---- UPDATE ----


type Msg
    = Sort Int Order
    | Filter Int String


update : Msg -> Model Msg -> Model Msg
update msg model =
    let
        value item =
            case item of
                Num num ->
                    String.fromInt num

                Str str ->
                    str

                View v ->
                    value v.value
    in
    case msg of
        Filter index str ->
            let
                filterConditions =
                    Dict.insert index str model.filterConditions

                contains items k v =
                    Array.get k items
                        |> Maybe.map (\item -> String.contains v (value item))
                        |> Maybe.withDefault False

                lines =
                    List.map
                        (\line ->
                            { line
                                | viewable =
                                    filterConditions
                                        |> Dict.map (contains line.items)
                                        |> Dict.toList
                                        |> List.all (\( _, v ) -> v)
                            }
                        )
                        model.lines
            in
            { model | lines = lines, filterConditions = filterConditions }

        Sort index order ->
            let
                cmp a b =
                    case ( a, b ) of
                        ( View v, _ ) ->
                            cmp v.value b

                        ( _, View v ) ->
                            cmp a v.value

                        ( Num _, Str _ ) ->
                            LT

                        ( Str _, Num _ ) ->
                            GT

                        ( Num n, Num m ) ->
                            compare n m

                        ( Str s, Str t ) ->
                            compare s t

                sortWith a b =
                    Maybe.map2 cmp (Array.get index a.items) (Array.get index b.items) |> Maybe.withDefault EQ

                sorted =
                    List.sortWith sortWith model.lines

                orderd =
                    case order of
                        Asc ->
                            sorted

                        Desc ->
                            List.reverse sorted
            in
            { model | lines = orderd, sortCondition = Just ( index, order ) }



---- VIEW ----


view : Model Msg -> Html Msg
view model =
    let
        label item =
            case item of
                Num num ->
                    String.fromInt num |> text

                Str str ->
                    text str

                View v ->
                    v.view

        filterInputField head index =
            if head.filtable then
                [ input [ onInput (Filter index) ] [] ]

            else
                []

        sortButtons head index =
            let
                isSelected order =
                    model.sortCondition
                        |> Maybe.map (\( idx, odr ) -> idx == index && order == odr)
                        |> Maybe.withDefault False

                ascIcon =
                    if isSelected Asc then
                        "▲"

                    else
                        "△"

                descIcon =
                    if isSelected Desc then
                        "▼"

                    else
                        "▽"
            in
            if head.sortable then
                [ span [ onClick (Sort index Asc) ] [ text ascIcon ], span [ onClick (Sort index Desc) ] [ text descIcon ] ]

            else
                []
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                (List.indexedMap
                    (\index ->
                        \head -> th [] (text head.label :: filterInputField head index ++ sortButtons head index)
                    )
                    model.heads
                )
            ]
        , tbody []
            (List.filterMap
                (\line ->
                    if line.viewable then
                        Just
                            (tr []
                                (line.items
                                    |> Array.map (\item -> td [] [ label item ])
                                    |> Array.toList
                                )
                            )

                    else
                        Nothing
                )
                model.lines
            )
        ]
