module ShuntingYard exposing (ShuntingYard, appendOperand, appendOperator, currentOperand, currentOperator, evaluate, extractExpression, extractOperatorStack, init, preemptiveEvaluate, replaceCurrentOperator)

import Operator exposing (OperatorType)
import RPNExpression as RPN exposing (RPNExpression)


type alias OperatorStack =
    List OperatorType


type ShuntingYard
    = ShuntingYard RPNExpression OperatorStack


init : ShuntingYard
init =
    ShuntingYard RPN.emptyExpression []


appendOperand : Int -> ShuntingYard -> ShuntingYard
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


evaluate : ShuntingYard -> Result String Int
evaluate (ShuntingYard expression operatorStack) =
    RPN.evaluate (List.foldl RPN.appendOperator expression operatorStack)


preemptiveEvaluate : ShuntingYard -> Result String Int
preemptiveEvaluate (ShuntingYard expression operatorStack) =
    expression
        |> List.reverse
        |> List.take 3
        |> List.reverse
        |> RPN.evaluate


currentOperand : ShuntingYard -> Int
currentOperand (ShuntingYard expression operatorStack) =
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
