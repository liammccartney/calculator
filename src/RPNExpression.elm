module RPNExpression exposing
    ( RPNExpression
    , Token
    , appendOperand
    , appendOperator
    , currentOperand
    , eagerEvaluate
    , emptyExpression
    , evaluate
    , evaluateImpl
    , toString
    )

import Operator exposing (OperatorType, calculate)


type Token
    = Operand String
    | Operator OperatorType


type alias RPNExpression =
    List Token


emptyExpression : RPNExpression
emptyExpression =
    []


appendOperand : String -> RPNExpression -> RPNExpression
appendOperand operand expression =
    expression ++ [ Operand operand ]


appendOperator : OperatorType -> RPNExpression -> RPNExpression
appendOperator operator expression =
    expression ++ [ Operator operator ]


currentOperand : RPNExpression -> String
currentOperand expression =
    let
        findOperand exp =
            case exp of
                [] ->
                    "0"

                (Operand operand) :: _ ->
                    operand

                _ :: operationTail ->
                    findOperand operationTail
    in
    expression
        |> List.reverse
        |> findOperand


evaluateImpl : List String -> RPNExpression -> List String
evaluateImpl stack exp =
    case exp of
        (Operator operator) :: expressionTail ->
            case stack of
                right :: left :: stackTail ->
                    expressionTail
                        |> evaluateImpl (calculate operator left right :: stackTail)

                _ ->
                    evaluateImpl [] expressionTail

        (Operand operand) :: expressionTail ->
            evaluateImpl (operand :: stack) expressionTail

        [] ->
            stack


evaluate : RPNExpression -> Result String String
evaluate expression =
    case evaluateImpl [] expression of
        result :: [] ->
            Ok result

        _ ->
            Err "Evaluation Failure"


eagerEvaluate : RPNExpression -> Result String String
eagerEvaluate expression =
    case evaluateImpl [] expression of
        result :: _ ->
            Ok result

        _ ->
            Err "Evaluation Failure"


toString : RPNExpression -> String
toString =
    String.join " "
        << List.map
            (\token ->
                case token of
                    Operand operand ->
                        operand

                    Operator operator ->
                        Operator.toString operator
            )
