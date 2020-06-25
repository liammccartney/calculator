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
    , replaceCurrentOperator
    , shiftCurrentOperandToExpression
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


replaceCurrentOperator : OperatorType -> ShuntingYard -> ShuntingYard
replaceCurrentOperator operator (ShuntingYard expression operatorStack) =
    case operatorStack of
        [] ->
            ShuntingYard expression [ operator ]

        head :: tail ->
            ShuntingYard expression (operator :: tail)


evaluate : ShuntingYard -> Result String String
evaluate (ShuntingYard expression operatorStack) =
    RPN.evaluate (List.foldl RPN.appendOperator expression operatorStack)


eagerEvaluate : ShuntingYard -> Result String String
eagerEvaluate (ShuntingYard expression operatorStack) =
    RPN.eagerEvaluate (List.foldl RPN.appendOperator expression operatorStack)


evaluateExpression : ShuntingYard -> Result String String
evaluateExpression (ShuntingYard expression operatorStack) =
    RPN.eagerEvaluate expression


shiftCurrentOperandToExpression : ShuntingYard -> ShuntingYard
shiftCurrentOperandToExpression (ShuntingYard expression operatorStack) =
    case operatorStack of
        [] ->
            ShuntingYard expression operatorStack

        operator :: operatorsTail ->
            ShuntingYard (RPN.appendOperator operator expression) operatorsTail


currentOperand : ShuntingYard -> String
currentOperand (ShuntingYard expression operatorStack) =
    RPN.currentOperand expression


currentOperator : ShuntingYard -> Maybe OperatorType
currentOperator (ShuntingYard _ operatorStack) =
    List.head operatorStack


extractExpression : ShuntingYard -> RPN.RPNExpression
extractExpression (ShuntingYard expression operatorsStack) =
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
