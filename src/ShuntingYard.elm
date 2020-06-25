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

{-| For lack of a better name this module is called ShuntingYard.
It is intended to be an implementation of the Shunting-yard algorithm, which is used to convert
infix expressions to postfix ones.
Rather than parsing an expression whole, this module defines functions that allow you to construct the expression
as you go.
-}

import Operator exposing (OperatorType)
import RPNExpression as RPN exposing (RPNExpression)


type alias OperatorStack =
    List OperatorType


type ShuntingYard
    = ShuntingYard RPNExpression OperatorStack


{-| init
Creates an empty ShuntingYard instance
-}
init : ShuntingYard
init =
    ShuntingYard RPN.emptyExpression []


{-| appendOperand
Appends a given operand directly to the underlying expression
-}
appendOperand : String -> ShuntingYard -> ShuntingYard
appendOperand operand (ShuntingYard expression operatorStack) =
    ShuntingYard (RPN.appendOperand operand expression) operatorStack


{-| appendOperator
Based on the precedence of the given operator, this function will shift operators from the operator stack to
the expression before adding the new operator to the stack.
This is intended to be how the infix expression becomes a postfix.
-}
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


{-| evaluate
Appends all leftover operators from the stack to the expression and evaluates it.
-}
evaluate : ShuntingYard -> Result String String
evaluate (ShuntingYard expression operatorStack) =
    RPN.evaluate (List.foldl RPN.appendOperator expression operatorStack)


{-| eagerEvaluate
Appends all leftover operators from the stack to the expression and evaluates it, eagerly.
It will return the result of as much of the expression as can be evaluated.
-}
eagerEvaluate : ShuntingYard -> Result String String
eagerEvaluate (ShuntingYard expression operatorStack) =
    RPN.eagerEvaluate (List.foldl RPN.appendOperator expression operatorStack)


{-| evaluateExpression
Directly evaluates the underlying expression without concern for the operator stack.
-}
evaluateExpression : ShuntingYard -> Result String String
evaluateExpression (ShuntingYard expression _) =
    RPN.eagerEvaluate expression


{-| currentOperand
Returns the current operand from the expression
-}
currentOperand : ShuntingYard -> String
currentOperand (ShuntingYard expression _) =
    RPN.currentOperand expression


{-| currentOperator
If there is at least operator at the head of the stack, return it.
-}
currentOperator : ShuntingYard -> Maybe OperatorType
currentOperator (ShuntingYard _ operatorStack) =
    List.head operatorStack


{-| extractExpression
Convenience function to accessing the underlying expression.
Useful for testing.
-}
extractExpression : ShuntingYard -> RPN.RPNExpression
extractExpression (ShuntingYard expression _) =
    expression


{-| extractOperatorStack
Convenience function to accessing the underlying operatorStack.
Useful for testing.
-}
extractOperatorStack : ShuntingYard -> OperatorStack
extractOperatorStack (ShuntingYard _ operatorStack) =
    operatorStack


{-| repeat
Appends the previous operator and operand to the expression
-}
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


{-| repeatWith
Creates a fresh expression with the given operand, the previous one, and the previous operator
-}
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


{-| toString
Maps the entire operation to a string.
Helpful for testing.
-}
toString : ShuntingYard -> String
toString (ShuntingYard expression operatorsStack) =
    List.foldl RPN.appendOperator expression operatorsStack |> RPN.toString
