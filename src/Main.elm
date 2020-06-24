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
import Mutator exposing (Mutator(..), mutate)
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
    | MutatorPressed Mutator
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

        MutatorPressed mutator ->
            case model.input of
                Empty ->
                    { model | input = InputLeft ("0" |> mutate mutator) }

                InputLeft operand ->
                    { model | input = InputLeft (operand |> mutate mutator) }

                NeedRight operand ->
                    { model | input = NeedRight (operand |> mutate mutator) }

                InputRight operand ->
                    { model | input = InputRight (operand |> mutate mutator) }

                EagerEvaluated operand ->
                    { model | input = EagerEvaluated (operand |> mutate mutator) }

                Evaluated operand ->
                    { model | input = Evaluated (operand |> mutate mutator) }

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
        if String.length newValString > 10 then
            current |> String.slice 0 10

        else
            newValString



-- VIEW


allOperands : List String
allOperands =
    [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" ]


inputToString : Input -> String
inputToString input =
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
    Html.div [ Html.css calculatorContainerStyles ]
        [ Html.div
            [ Html.css displayTotalCss ]
            [ Html.text (inputToString model.input) ]
        , Html.div [ Html.css buttonsContainerStyles ]
            [ -- TODO: Replace with Clear
              mutatorButton Negate
            , mutatorButton Negate
            , mutatorButton Percentile
            , operatorButton Divide
            , operandButton "7"
            , operandButton "8"
            , operandButton "9"
            , operatorButton Multiply
            , operandButton "4"
            , operandButton "5"
            , operandButton "6"
            , operatorButton Subtract
            , operandButton "1"
            , operandButton "2"
            , operandButton "3"
            , operatorButton Add
            , zeroButton
            , mutatorButton AppendDecimalPoint
            , equalsButton
            ]
        ]


grey : Css.Color
grey =
    Css.hex "6b6a69"


yellow : Css.Color
yellow =
    Css.hex "e0a225"


calculatorButton : Msg -> List Css.Style -> Html Msg
calculatorButton msg styles =
    let
        displayText =
            case msg of
                OperandPressed operand ->
                    operand

                OperatorPressed operator ->
                    Operator.toString operator

                MutatorPressed mutator ->
                    Mutator.toString mutator

                EqualsPressed ->
                    "="
    in
    Html.button
        [ onClick msg
        , Html.css
            ([ Css.flexBasis <|
                Css.pct 25
             , Css.padding2 Css.zero (Css.px 8)
             , Css.height (Css.px 65)
             , Css.textAlign Css.center
             , Css.fontSize (Css.px 24)
             , Css.color <| Css.hex "fff"
             , Css.backgroundColor grey
             , Css.border3 (Css.px 0.5) Css.solid (Css.hex "000")
             , Css.cursor Css.pointer
             , Css.focus
                [ Css.outline Css.none
                ]
             ]
                ++ styles
            )
        ]
        [ Html.text displayText ]


operandButton : String -> Html Msg
operandButton operand =
    calculatorButton (OperandPressed operand) []


operatorButton : OperatorType -> Html Msg
operatorButton operator =
    calculatorButton (OperatorPressed operator) [ Css.backgroundColor yellow ]


mutatorButton : Mutator -> Html Msg
mutatorButton mutator =
    calculatorButton (MutatorPressed mutator) []


zeroButton : Html Msg
zeroButton =
    calculatorButton (OperandPressed "0")
        [ Css.flexGrow (Css.int 1)
        , Css.borderBottomLeftRadius (Css.px 5)
        ]


isZero : String -> Bool
isZero operand =
    operand |> String.toFloat |> Maybe.withDefault 0 |> (==) 0


equalsButton : Html Msg
equalsButton =
    calculatorButton EqualsPressed
        [ Css.backgroundColor yellow
        , Css.borderBottomRightRadius (Css.px 5)
        ]



-- STYLES


calculatorContainerStyles : List Css.Style
calculatorContainerStyles =
    [ Css.margin2 (Css.px 0) Css.auto
    , Css.width <| Css.px 300
    , Css.paddingTop (Css.px 80)
    , Css.fontFamilies [ "Helvetica" ]
    , Css.fontWeight Css.lighter
    ]


displayTotalCss : List Css.Style
displayTotalCss =
    [ Css.margin2 Css.zero Css.auto
    , Css.fontSize (Css.px 48)
    , Css.textAlign Css.right
    , Css.padding4 (Css.px 15) (Css.px 10) (Css.px 0) (Css.px 5)
    , Css.backgroundColor (Css.hex "595757")
    , Css.color (Css.hex "fff")
    , Css.borderRadius4 (Css.px 5) (Css.px 5) (Css.px 0) (Css.px 0)
    ]


buttonsContainerStyles : List Css.Style
buttonsContainerStyles =
    [ Css.displayFlex
    , Css.flexWrap Css.wrap
    , Css.margin2 (Css.px 0) Css.auto
    , Css.width <| Css.px 300
    ]
