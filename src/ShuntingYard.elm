module ShuntingYard exposing
    ( ShuntingYard
    , appendOperand
    , appendOperator
    , currentOperand
    , currentOperator
    , eagerEvaluate
    , evaluate
    , evaluateExpression
    , extractExpression
    , extractOperatorStack
    , init
    , repeat
    , repeatWith
    , toString
    )

import Operator exposing (OperatorType)
import RPNExpression as RPN exposing (RPNExpression)


type alias OperatorStack =
    List OperatorType


type ShuntingYard
    = ShuntingYard RPNExpression OperatorStack


init : ShuntingYard
init =
    ShuntingYard RPN.emptyExpression []


appendOperand : String -> ShuntingYard -> ShuntingYard
appendOperand operand (ShuntingYard expression operatorStack) =
    ShuntingYard (RPN.appendOperand operand expression) operatorStack


appendOperator : OperatorType -> ShuntingYard -> ShuntingYard
appendOperator operator (ShuntingYard expression operatorStack) =
    let
        operatorsToMoveToExpression =
            Operator.findPrecedentOperators operatorStack operator

        remainingOperators =
            Operator.findLesserOperators operatorStack operator

        newExpression =
            List.foldl RPN.appendOperator expression operatorsToMoveToExpression
    in
    ShuntingYard newExpression (operator :: remainingOperators)


evaluate : ShuntingYard -> Result String String
evaluate (ShuntingYard expression operatorStack) =
    RPN.evaluate (List.foldl RPN.appendOperator expression operatorStack)


eagerEvaluate : ShuntingYard -> Result String String
eagerEvaluate (ShuntingYard expression operatorStack) =
    RPN.eagerEvaluate (List.foldl RPN.appendOperator expression operatorStack)


evaluateExpression : ShuntingYard -> Result String String
evaluateExpression (ShuntingYard expression _) =
    RPN.eagerEvaluate expression


currentOperand : ShuntingYard -> String
currentOperand (ShuntingYard expression _) =
    RPN.currentOperand expression


currentOperator : ShuntingYard -> Maybe OperatorType
currentOperator (ShuntingYard _ operatorStack) =
    List.head operatorStack


extractExpression : ShuntingYard -> RPN.RPNExpression
extractExpression (ShuntingYard expression _) =
    expression


extractOperatorStack : ShuntingYard -> OperatorStack
extractOperatorStack (ShuntingYard _ operatorStack) =
    operatorStack


repeat : ShuntingYard -> ShuntingYard
repeat (ShuntingYard expression operatorStack) =
    let
        operand =
            RPN.currentOperand expression
    in
    case List.head operatorStack of
        Just operator ->
            ShuntingYard expression operatorStack
                |> appendOperator operator
                |> appendOperand operand

        Nothing ->
            ShuntingYard expression operatorStack


repeatWith : String -> ShuntingYard -> ShuntingYard
repeatWith operand (ShuntingYard expression operatorStack) =
    let
        currOperand =
            RPN.currentOperand expression
    in
    case List.head operatorStack of
        Just operator ->
            init
                |> appendOperand operand
                |> appendOperand currOperand
                |> appendOperator operator

        Nothing ->
            ShuntingYard expression operatorStack


toString : ShuntingYard -> String
toString (ShuntingYard expression operatorsStack) =
    List.foldl RPN.appendOperator expression operatorsStack |> RPN.toString
