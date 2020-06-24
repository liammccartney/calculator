module Main exposing (..)

import Browser
import Css
import Css.Global
import Decimal
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed
import List.Extra
import Operator exposing (OperatorType(..))
import RPNExpression exposing (..)
import ShuntingYard exposing (ShuntingYard)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view >> Html.toUnstyled }



-- MODEL


type Input
    = InputLeft String
    | NeedRight String
    | InputRight String
    | EagerEvaluated String
    | Evaluated String
    | Empty


type alias Model =
    { input : Input
    , yard : ShuntingYard
    , previous : ShuntingYard
    , evaluated : Result String String
    }


init : Model
init =
    Model Empty ShuntingYard.init ShuntingYard.init (Err "empty")



-- UPDATE


type Msg
    = OperandPressed String
    | OperatorPressed OperatorType
    | EqualsPressed


update : Msg -> Model -> Model
update msg model =
    case msg of
        OperandPressed operand ->
            case model.input of
                Empty ->
                    { model | input = InputLeft operand }

                InputLeft currOperand ->
                    { model | input = operand |> incrementOperand currOperand |> InputLeft }

                NeedRight _ ->
                    { model | input = operand |> InputRight }

                InputRight currOperand ->
                    { model | input = operand |> incrementOperand currOperand |> InputRight }

                EagerEvaluated result ->
                    { model | input = InputRight operand }

                Evaluated result ->
                    { model | input = InputLeft operand, yard = ShuntingYard.init }

        OperatorPressed operator ->
            let
                ( yard, input ) =
                    case model.input of
                        Empty ->
                            ( model.yard
                                |> ShuntingYard.appendOperand "0"
                                |> ShuntingYard.appendOperator operator
                            , NeedRight "0"
                            )

                        InputLeft left ->
                            ( model.yard
                                |> ShuntingYard.appendOperand left
                                |> ShuntingYard.appendOperator operator
                            , NeedRight left
                            )

                        NeedRight left ->
                            ( model.yard
                                |> ShuntingYard.replaceCurrentOperator operator
                            , NeedRight left
                            )

                        InputRight right ->
                            ( model.yard
                                |> ShuntingYard.appendOperand right
                                |> ShuntingYard.appendOperator operator
                            , NeedRight right
                            )

                        EagerEvaluated result ->
                            ( model.yard
                                |> ShuntingYard.replaceCurrentOperator operator
                            , EagerEvaluated result
                            )

                        Evaluated result ->
                            ( model.yard
                                |> ShuntingYard.appendOperator operator
                            , NeedRight result
                            )
            in
            case ShuntingYard.preemptiveEvaluate yard of
                Ok result ->
                    { model | yard = yard, input = EagerEvaluated result, evaluated = Ok result }

                Err _ ->
                    { model | yard = yard, input = input, evaluated = Err "bad eval" }

        EqualsPressed ->
            let
                ( yard, input ) =
                    case model.input of
                        Empty ->
                            ( model.yard
                            , Evaluated "0"
                            )

                        InputLeft operand ->
                            case ShuntingYard.currentOperator model.previous of
                                Just operator ->
                                    ( ShuntingYard.init
                                        |> ShuntingYard.appendOperand operand
                                        |> ShuntingYard.appendOperand (ShuntingYard.currentOperand model.previous)
                                        |> ShuntingYard.appendOperator operator
                                    , InputLeft operand
                                    )

                                Nothing ->
                                    ( model.yard, Evaluated operand )

                        NeedRight operand ->
                            ( model.yard
                                |> ShuntingYard.appendOperand operand
                            , NeedRight operand
                            )

                        InputRight operand ->
                            ( model.yard
                                |> ShuntingYard.appendOperand operand
                            , InputRight operand
                            )

                        EagerEvaluated result ->
                            ( model.yard
                                |> ShuntingYard.shiftCurrentOperandToExpression
                                |> ShuntingYard.appendOperand result
                            , EagerEvaluated result
                            )

                        Evaluated result ->
                            case ShuntingYard.currentOperator model.yard of
                                Just operator ->
                                    ( model.yard
                                        |> ShuntingYard.appendOperator operator
                                        |> ShuntingYard.appendOperand (ShuntingYard.currentOperand model.yard)
                                    , Evaluated result
                                    )

                                Nothing ->
                                    ( model.yard, Evaluated result )
            in
            case ShuntingYard.evaluate yard of
                Ok result ->
                    { model | input = Evaluated result, yard = yard, evaluated = Ok result, previous = yard }

                Err m ->
                    { model | input = input, evaluated = Err m }


incrementOperand : String -> String -> String
incrementOperand current new =
    if current == "0" then
        new

    else
        let
            newValString =
                current ++ new
        in
        if String.length newValString > 16 then
            current |> String.slice 0 16

        else
            newValString



-- VIEW


allOperands : List String
allOperands =
    [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" ]


viewEvaluation : Input -> String
viewEvaluation input =
    case input of
        Empty ->
            "0"

        InputLeft operand ->
            operand

        NeedRight operand ->
            operand

        InputRight operand ->
            operand

        EagerEvaluated result ->
            result

        Evaluated result ->
            result


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Html.css displayTotalCss ]
            [ Html.text (viewEvaluation model.input)
            ]
        , List.append (List.map cardView allOperands)
            [ cardViewOperator Add "+"
            , cardViewOperator Subtract "-"
            , cardViewOperator Multiply "X"
            , cardViewOperator Divide "÷"
            , cardViewEquals
            ]
            |> Html.Styled.Keyed.node "div"
                [ Html.css css ]
        ]


grey : Css.Color
grey =
    Css.hex "d8dee9"


displayTotalCss : List Css.Style
displayTotalCss =
    [ Css.width (Css.px 400)
    , Css.margin2 Css.zero Css.auto
    , Css.fontSize (Css.px 48)
    , Css.textAlign Css.right
    , Css.paddingRight (Css.px 60)
    , Css.paddingTop (Css.px 60)
    ]


cardView : String -> ( String, Html Msg )
cardView operand =
    ( "card" ++ operand
    , Html.div
        [ onClick (OperandPressed operand) ]
        [ Html.text operand ]
    )


isZero : String -> Bool
isZero operand =
    operand |> String.toFloat |> Maybe.withDefault 0 |> (==) 0


cardViewOperator : OperatorType -> String -> ( String, Html Msg )
cardViewOperator operator symbol =
    ( "card" ++ symbol
    , Html.div
        [ onClick (OperatorPressed operator) ]
        [ Html.text symbol ]
    )


cardViewEquals : ( String, Html Msg )
cardViewEquals =
    ( "card" ++ "="
    , Html.div
        [ onClick EqualsPressed ]
        [ Html.text "=" ]
    )



-- STYLES


css : List Css.Style
css =
    [ Css.displayFlex
    , Css.flexWrap Css.wrap
    , Css.margin2 (Css.px 0) Css.auto
    , Css.width <| Css.px 400
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
