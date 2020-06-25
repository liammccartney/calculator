module Main exposing (..)

{-| The entry point for the calulated app!
-}

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
                -- There is no working operand, so the incoming one becomes it.
                Empty ->
                    { model | input = Operand operand }

                -- We're editing an operand, so the incoming one gets appended to it.
                Operand currOperand ->
                    { model | input = Operand (appendToOperand currOperand operand) }

                -- We have an operator, so it needs to get added to our expression
                -- And we change state to editing our new operand
                Operator operator ->
                    { model
                        | yard =
                            model.yard
                                |> ShuntingYard.appendOperator operator
                        , input = Operand operand
                    }

                -- We just evaluated an expression, we store this in the previous cache
                -- So we can access it if we want to combine our new operand with our previous
                -- expression.
                Evaluated result ->
                    { model | input = Operand operand, previous = Just result }

        OperatorPressed operator ->
            case model.input of
                -- We're not editing the expression yet, so we start with the operator
                -- This is a bit odd, but since we default to 0 if the expression is empty of
                -- operands, this places out just fine.
                Empty ->
                    { model | input = Operator operator }

                -- We're working with an operand, so it's time to shift that to the expression
                -- And start working with this operator
                Operand currOperand ->
                    { model
                        | yard =
                            model.yard
                                |> ShuntingYard.appendOperand currOperand
                        , input = Operator operator
                        , previous = Nothing
                    }

                -- Looks like we changed our mind and we need to change operators
                -- without affecting the expression
                Operator _ ->
                    { model | input = Operator operator }

                -- We just evaluated an expression, and we want to use that expression again
                -- this new operator
                Evaluated yard ->
                    { model
                        | input = Operator operator
                        , yard = yard
                    }

        MutatorPressed mutator ->
            case model.input of
                -- There is no operand to muate, so we default to 0
                Empty ->
                    { model | input = Operand (mutate mutator "0") }

                -- Mutate the working operand directly without affecting the
                -- underlying expression
                Operand operand ->
                    { model | input = Operand (mutate mutator operand) }

                -- We're working with an operator, so in order to mutate the operand
                -- we fetch it from the expression first
                -- Then we set the mutated result as the working operand
                Operator _ ->
                    let
                        currOperand =
                            model.yard |> ShuntingYard.currentOperand
                    in
                    { model | input = Operand (mutate mutator currOperand) }

                -- We just evaluated an expression and we want to mutate the result
                -- We re-evaluate the expression, assuming that it will work b/c there shouldn't
                -- be a way that we got into this state with an invalid expression
                -- Then we set the working operand to be the mutated result
                Evaluated yard ->
                    let
                        result =
                            yard |> ShuntingYard.evaluate |> Result.withDefault "0"
                    in
                    { model | input = Operand (mutate mutator result) }

        EqualsPressed ->
            case model.input of
                -- We don't have a working operand, this means we want to repeat the previous
                -- expression
                Empty ->
                    { model
                        | yard =
                            model.yard
                                |> ShuntingYard.repeat
                        , input = Empty
                    }

                -- We have an operand, we might want to repeat the previous operation
                -- except with this operand. If we have a previous expression, that's what we'll do
                -- Otherwise it's time to start a new expression.
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

                -- We have an operator, which means we want to add this operator to the expression
                -- and then add the result of the expression to itself.
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

                -- We just evaluated an expression, we want to repeat it
                -- We also want to unset the previous operation so we can start fresh with a new one
                -- if we press another operand
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


{-| appendToOperand
Appends and operand to another one.
Constrains the result to 10 characters long to avoid values longer than the screen can display
-}
appendToOperand : String -> String -> String
appendToOperand current new =
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


{-| display
Depending on the current input state, render an evaluation of the expression.
-}
display : Model -> String
display model =
    case model.input of
        -- No evaluation necessary, we're working with an operand, so we just show it.
        Operand operand ->
            operand

        -- We just submitted an expression, so it must be evaluated
        -- If it can't be, show the current operand
        Evaluated yard ->
            case ShuntingYard.eagerEvaluate yard of
                Ok result ->
                    result

                Err _ ->
                    ShuntingYard.currentOperand yard

        -- We're working with an operator, if applying that operator to the expression
        -- makes any part of it possible to be evaluated, display that result
        -- Otherwise the current operand is what we want.
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

        -- No working operand. If the expression can be evaluated, show that.
        -- Otherwise the current operand.
        Empty ->
            case ShuntingYard.evaluate model.yard of
                Ok result ->
                    result

                Err _ ->
                    ShuntingYard.currentOperand model.yard


{-| view
Defines the layout & styling of the calculator interface
-}
view : Model -> Html Msg
view model =
    Html.div [ Html.css calculatorContainerStyles ]
        [ Html.div
            [ Html.css runningTotalStyles ]
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


{-| grey
Convenience value for the color grey
-}
grey : Css.Color
grey =
    Css.hex "6b6a69"


{-| yellow
Convenience value for the color yellow
-}
yellow : Css.Color
yellow =
    Css.hex "e0a225"


{-| calculatorButton
Defines how to render a button in the calculator interface.
The rendered text depends on the supplied message.
-}
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


{-| operandButton
Specification of the calculatorButton for pressing operands
-}
operandButton : String -> Html Msg
operandButton operand =
    calculatorButton (OperandPressed operand) []


{-| operatorButton
Specification of the calculatorButton for pressing operators
-}
operatorButton : OperatorType -> Html Msg
operatorButton operator =
    calculatorButton (OperatorPressed operator) [ Css.backgroundColor yellow ]


{-| mutatorButton
Specification of the calculatorButton for pressing mutators
-}
mutatorButton : Mutator -> Html Msg
mutatorButton mutator =
    calculatorButton (MutatorPressed mutator) []


{-| zeroButton
Special operand button, the zero button is wider than the rest of the operand buttons.
-}
zeroButton : Html Msg
zeroButton =
    calculatorButton (OperandPressed "0")
        [ Css.flexGrow (Css.int 1)
        , Css.borderBottomLeftRadius (Css.px 5)
        ]


{-| equalsButton
Specification of the calculatorButton for pressing equals
-}
equalsButton : Html Msg
equalsButton =
    calculatorButton EqualsPressed
        [ Css.backgroundColor yellow
        , Css.borderBottomRightRadius (Css.px 5)
        ]


{-| clearButton
Specification of the calculatorButton for pressing all clear
-}
clearButton : Input -> Html Msg
clearButton input =
    calculatorButton AllClearPressed []



-- STYLES


{-| calculatorContainerStyles
Syles for the container div for the calculator interface.
-}
calculatorContainerStyles : List Css.Style
calculatorContainerStyles =
    [ Css.margin2 (Css.px 0) Css.auto
    , Css.width <| Css.px 300
    , Css.paddingTop (Css.px 80)
    , Css.fontFamilies [ "Helvetica" ]
    , Css.fontWeight Css.lighter
    ]


{-| runningTotalStyles
Syles for the div that renders the running total of the expression, or the
current operand
-}
runningTotalStyles : List Css.Style
runningTotalStyles =
    [ Css.margin2 Css.zero Css.auto
    , Css.fontSize (Css.px 48)
    , Css.textAlign Css.right
    , Css.padding4 (Css.px 15) (Css.px 10) (Css.px 0) (Css.px 5)
    , Css.backgroundColor (Css.hex "595757")
    , Css.color (Css.hex "fff")
    , Css.borderRadius4 (Css.px 5) (Css.px 5) (Css.px 0) (Css.px 0)
    ]


{-| buttonsContainerStyles
Styles for the calculator's inner container that holds all the buttons.
-}
buttonsContainerStyles : List Css.Style
buttonsContainerStyles =
    [ Css.displayFlex
    , Css.flexWrap Css.wrap
    , Css.margin2 (Css.px 0) Css.auto
    , Css.width <| Css.px 300
    ]
