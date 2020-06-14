module Main exposing
    ( Model
    , init
    , view
    )

import Browser
import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed
import Placeholders.Square exposing (Square)


main =
    Browser.sandbox { init = init, update = update, view = view >> Html.toUnstyled }



-- MODEL


type Operator
    = NoOp
    | Divide
    | Multiply
    | Subtract
    | Add
    | Equals


type alias Model =
    { lhs : Int
    , rhs : Int
    , operator : Operator
    }


grey : Css.Color
grey =
    Css.hex "d8dee9"


init : Model
init =
    { lhs = 0
    , rhs = 0
    , operator = NoOp
    }



-- VIEW


allOperands =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 ]


view : Model -> Html Msg
view { lhs, rhs, operator } =
    Html.div []
        [ Html.div [ Html.css displayTotalCss ]
            [ case ( lhs, rhs ) of
                ( left, 0 ) ->
                    Html.text (String.fromInt left)

                ( left, right ) ->
                    Html.text (String.fromInt right)
            ]
        , List.append (List.map cardView allOperands)
            [ cardViewOperator Add "+"
            , cardViewOperator Subtract "-"
            , cardViewOperator Multiply "X"
            , cardViewOperator Divide "÷"
            , cardViewOperator Equals "="
            , clear
            ]
            |> Html.Styled.Keyed.node "div"
                [ Html.css css ]
        ]


displayTotalCss =
    [ Css.width (Css.pct 80)
    , Css.margin2 Css.zero Css.auto
    , Css.fontSize (Css.px 48)
    , Css.textAlign Css.center
    ]


cardView : Int -> ( String, Html Msg )
cardView operand =
    ( "card" ++ String.fromInt operand
    , Html.div
        [ onClick (OperandPressed operand) ]
        [ Html.text (String.fromInt operand) ]
    )


clear : ( String, Html Msg )
clear =
    ( "card clear"
    , Html.div
        [ onClick ClearPressed ]
        [ Html.text "C" ]
    )


cardViewOperator : Operator -> String -> ( String, Html Msg )
cardViewOperator operator symbol =
    ( "card" ++ symbol
    , Html.div
        [ onClick (OperatorPressed operator) ]
        [ Html.text symbol ]
    )


cardViewOperatorEquals : String -> ( String, Html Msg )
cardViewOperatorEquals operator =
    ( "card" ++ operator
    , Html.div
        [ onClick (OperatorPressed Equals) ]
        [ Html.text operator ]
    )



-- STYLES


css : List Css.Style
css =
    [ Css.displayFlex
    , Css.flexWrap Css.wrap
    , Css.margin2 (Css.px 80) Css.auto
    , Css.width <| Css.pct 80
    , Css.paddingTop <| Css.rem 1
    , Css.Global.descendants
        [ Css.Global.selector "> div"
            [ Css.flexBasis <| Css.pct 20
            , Css.padding2 Css.zero (Css.px 8)
            , Css.height (Css.px 50)
            , Css.textAlign Css.center
            , Css.fontSize (Css.px 24)
            , Css.margin (Css.rem 1)
            , Css.backgroundColor grey
            ]
        , Css.Global.selector "> div > div"
            [ Css.width <| Css.pct 100
            ]
        , Css.Global.selector ".zero"
            [ Css.flexGrow (Css.int 1)
            ]
        ]
    ]


type Msg
    = OperandPressed Int
    | OperatorPressed Operator
    | ClearPressed


inputsToString : List Int -> String
inputsToString inputs =
    inputs
        |> List.map String.fromInt
        |> String.join ""


update : Msg -> Model -> Model
update msg model =
    let
        { lhs, rhs, operator } =
            model
    in
    case msg of
        OperandPressed operand ->
            case ( lhs, rhs, operator ) of
                ( 0, 0, op ) ->
                    { model | lhs = operand }

                ( left, 0, NoOp ) ->
                    { model | lhs = left * 10 + operand }

                ( left, 0, op ) ->
                    { model | rhs = operand }

                ( left, right, op ) ->
                    { model | rhs = right * 10 + operand }

        OperatorPressed op ->
            case ( lhs, rhs, op ) of
                ( left, right, Equals ) ->
                    { model | lhs = applyOperator lhs rhs operator, rhs = 0, operator = NoOp }

                ( left, right, _ ) ->
                    { model | lhs = applyOperator lhs rhs operator, rhs = 0, operator = op }

        ClearPressed ->
            init


applyOperator lhs rhs operator =
    case operator of
        NoOp ->
            rhs

        Add ->
            lhs + rhs

        Multiply ->
            lhs * rhs

        Subtract ->
            lhs - rhs

        Divide ->
            lhs // rhs

        Equals ->
            rhs
