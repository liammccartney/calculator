module RPNExpression exposing (RPNExpression, Token, appendOperand, appendOperator, currentOperand, emptyExpression, evaluate, toString)

import Operator exposing (OperatorType, calculate)


type Token
    = Operand Int
    | Operator OperatorType


type alias RPNExpression =
    List Token


emptyExpression : RPNExpression
emptyExpression =
    []


appendOperand : Int -> RPNExpression -> RPNExpression
appendOperand operand expression =
    expression ++ [ Operand operand ]


appendOperator : OperatorType -> RPNExpression -> RPNExpression
appendOperator operator expression =
    expression ++ [ Operator operator ]


currentOperand : RPNExpression -> Int
currentOperand expression =
    let
        findOperand exp =
            case exp of
                [] ->
                    0

                (Operand operand) :: _ ->
                    operand

                _ :: operationTail ->
                    findOperand operationTail
    in
    expression
        |> List.reverse
        |> findOperand


evaluate : RPNExpression -> Result String Int
evaluate expression =
    let
        evaluateFunc exp stack =
            case exp of
                (Operator operator) :: expressionTail ->
                    case stack of
                        right :: left :: stackTail ->
                            evaluateFunc expressionTail (calculate operator left right :: stackTail)

                        _ ->
                            Err "Evaluation Failure: Too Few Operands"

                (Operand operand) :: expressionTail ->
                    evaluateFunc expressionTail (operand :: stack)

                [] ->
                    case stack of
                        result :: [] ->
                            Ok result

                        _ ->
                            Err "Evaluation Failure: Too Few Operators"
    in
    if List.length expression >= 3 then
        evaluateFunc expression []

    else
        Err "Evaluation Failure: Expression Too Short"


toString : RPNExpression -> String
toString =
    String.join " "
        << List.map
            (\token ->
                case token of
                    Operand int ->
                        String.fromInt int

                    Operator operator ->
                        Operator.toString operator
            )
