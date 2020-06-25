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

{-| This module exposes types and functions to manipulate a Reverse Polish Notation expression
-}

import Operator exposing (OperatorType, calculate)


type Token
    = Operand String
    | Operator OperatorType


type alias RPNExpression =
    List Token


{-| emptyExpression
Returns an empty RPNExpression.
This is where you get started when building an expression
-}
emptyExpression : RPNExpression
emptyExpression =
    []


{-| appendOperand
Appends a given operand string to the expression as a token
-}
appendOperand : String -> RPNExpression -> RPNExpression
appendOperand operand expression =
    expression ++ [ Operand operand ]


{-| appendOperator
Appends a given operator to the expression as a token
-}
appendOperator : OperatorType -> RPNExpression -> RPNExpression
appendOperator operator expression =
    expression ++ [ Operator operator ]


{-| currentOperand
Returns the current working operand for the expression.
If the expression is empty it returns 0.
-}
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


{-| evaluateImpl
This is the internal implementation of how an expression is recursively evaluted.
It traverses the given expression, popping any operands to a stack. When an
operator is encountered, the first two elements of the stack are evaluated with the operator.
The result is placed back onto the stack.
A successful evaluation will return an stack with one element.
-}
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


{-| evaluate
Returns the result of a succesful evaluation. Otherwise it returns an Error.
-}
evaluate : RPNExpression -> Result String String
evaluate expression =
    case evaluateImpl [] expression of
        result :: [] ->
            Ok result

        _ ->
            Err "Evaluation Failure"


{-| eagerEvaluate
Evaluates as much of the expression as possible, returning that result.
Any remaining tokens in the expression are ignored.
Returns an error if there where no possible evaluations to be made.
-}
eagerEvaluate : RPNExpression -> Result String String
eagerEvaluate expression =
    case evaluateImpl [] expression of
        result :: _ ->
            Ok result

        _ ->
            Err "Evaluation Failure"


{-| toString
Converts the expression to a string.
This is helpful in a few scenarios, speficially testing.
-}
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
