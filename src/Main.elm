module Main exposing (..)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events exposing (onClick)
import Mutator exposing (Mutator(..), mutate)
import Operator exposing (OperatorType(..))
import ShuntingYard exposing (ShuntingYard)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view >> Html.toUnstyled }



-- MODEL


type Input
    = Operand String
    | Operator OperatorType
    | Evaluated ShuntingYard
    | Empty


type alias Model =
    { input : Input
    , yard : ShuntingYard
    , previous : Maybe ShuntingYard
    }


init : Model
init =
    Model Empty ShuntingYard.init Nothing



-- UPDATE


type Msg
    = OperandPressed String
    | OperatorPressed OperatorType
    | MutatorPressed Mutator
    | EqualsPressed
    | AllClearPressed


update : Msg -> Model -> Model
update msg model =
    case msg of
        OperandPressed operand ->
            case model.input of
                Empty ->
                    { model | input = Operand operand }

                Operand currOperand ->
                    { model | input = Operand (incrementOperand currOperand operand) }

                Operator operator ->
                    { model
                        | yard =
                            model.yard
                                |> ShuntingYard.appendOperator operator
                        , input = Operand operand
                    }

                Evaluated result ->
                    { model | input = Operand operand, previous = Just result }

        OperatorPressed operator ->
            case model.input of
                Empty ->
                    { model | input = Operator operator }

                Operand currOperand ->
                    { model
                        | yard =
                            model.yard
                                |> ShuntingYard.appendOperand currOperand
                        , input = Operator operator
                        , previous = Nothing
                    }

                Operator _ ->
                    { model | input = Operator operator }

                Evaluated yard ->
                    { model
                        | input = Operator operator
                        , yard = yard
                    }

        MutatorPressed mutator ->
            case model.input of
                Empty ->
                    model

                Operand operand ->
                    { model | input = Operand (mutate mutator operand) }

                Operator _ ->
                    let
                        currOperand =
                            model.yard |> ShuntingYard.currentOperand
                    in
                    { model | input = Operand (mutate mutator currOperand) }

                Evaluated yard ->
                    let
                        result =
                            yard |> ShuntingYard.evaluate |> Result.withDefault "0"
                    in
                    { model | input = Operand (mutate mutator result) }

        EqualsPressed ->
            case model.input of
                Empty ->
                    { model
                        | yard =
                            model.yard
                                |> ShuntingYard.repeat
                        , input = Empty
                    }

                Operand operand ->
                    case model.previous of
                        Just previous ->
                            let
                                yard =
                                    previous
                                        |> ShuntingYard.repeatWith operand
                            in
                            { model
                                | yard = ShuntingYard.init
                                , input = Evaluated yard
                                , previous = Just yard
                            }

                        Nothing ->
                            { model
                                | yard = ShuntingYard.init
                                , input =
                                    Evaluated <|
                                        ShuntingYard.appendOperand operand <|
                                            model.yard
                            }

                Operator operator ->
                    let
                        yard =
                            model.yard |> ShuntingYard.appendOperator operator
                    in
                    case ShuntingYard.evaluateExpression yard of
                        Ok result ->
                            { model
                                | yard =
                                    model.yard
                                        |> ShuntingYard.appendOperator operator
                                        |> ShuntingYard.appendOperand result
                                , input = Empty
                                , previous = Nothing
                            }

                        Err _ ->
                            { model
                                | yard =
                                    model.yard
                                        |> ShuntingYard.appendOperator operator
                                , input = Empty
                                , previous = Nothing
                            }

                Evaluated yard ->
                    { model
                        | yard =
                            yard
                                |> ShuntingYard.repeat
                        , input = Empty
                        , previous = Nothing
                    }

        AllClearPressed ->
            init


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


display : Model -> String
display model =
    case model.input of
        Operand operand ->
            operand

        Evaluated yard ->
            case ShuntingYard.eagerEvaluate yard of
                Ok result ->
                    result

                Err _ ->
                    ShuntingYard.currentOperand yard

        Operator operator ->
            let
                yard =
                    model.yard |> ShuntingYard.appendOperator operator
            in
            case ShuntingYard.evaluateExpression yard of
                Ok result ->
                    result

                Err _ ->
                    ShuntingYard.currentOperand yard

        Empty ->
            case ShuntingYard.evaluate model.yard of
                Ok result ->
                    result

                Err _ ->
                    ShuntingYard.currentOperand model.yard


view : Model -> Html Msg
view model =
    Html.div [ Html.css calculatorContainerStyles ]
        [ Html.div
            [ Html.css displayTotalCss ]
            [ Html.text (display model) ]
        , Html.div [ Html.css buttonsContainerStyles ]
            [ clearButton model.input
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

                AllClearPressed ->
                    "AC"
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


clearButton : Input -> Html Msg
clearButton input =
    calculatorButton AllClearPressed []



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
