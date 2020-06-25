module Operator exposing (OperatorType(..), calculate, findLesserOperators, findPrecedentOperators, toString)

{-| This module is for managing operators, specifically Add, Subtract, Multiply, Divide.
It exposes functions to determin operator precedence, as well as calculating an expression with any of the operators.
It uses the Decimal library for arbitrary precision calculations.
-}

import Decimal
import List.Extra


type OperatorType
    = Add
    | Subtract
    | Multiply
    | Divide


{-| findPrecedentOperators
Returns all operators in a list that are of the same or higher precedence than a given operator.
-}
findPrecedentOperators : List OperatorType -> OperatorType -> List OperatorType
findPrecedentOperators operatorsList operator =
    List.Extra.takeWhile (\op -> takesPrecedence op operator) operatorsList


{-| findLesserOperators
Return a list of operators starting with first operator in the list of lesser precedence than the given operator to the last.
-}
findLesserOperators : List OperatorType -> OperatorType -> List OperatorType
findLesserOperators operatorsList operator =
    List.Extra.dropWhile (\op -> takesPrecedence op operator) operatorsList


{-| precedence
Defines vales used to compare operator precedence.
-}
precedence : OperatorType -> Int
precedence operator =
    case operator of
        Multiply ->
            2

        Divide ->
            2

        Add ->
            1

        Subtract ->
            1


{-| calculate
Evaluates a given operator with a left and right hand side.
-}
calculate : OperatorType -> String -> String -> String
calculate operator left right =
    let
        lhs =
            left |> Decimal.fromString |> Maybe.withDefault Decimal.zero

        rhs =
            right |> Decimal.fromString |> Maybe.withDefault Decimal.zero

        result =
            case operator of
                Add ->
                    Decimal.add lhs rhs |> Decimal.toString

                Multiply ->
                    Decimal.mul lhs rhs |> Decimal.toString

                Subtract ->
                    Decimal.sub lhs rhs |> Decimal.toString

                Divide ->
                    if isZero right then
                        "Infinity"

                    else
                        rhs
                            |> Decimal.fastdiv lhs
                            |> Maybe.withDefault Decimal.zero
                            |> Decimal.toString
    in
    -- Remove .0 from the end of the result to keep things clean
    if String.endsWith ".0" result then
        String.slice 0 -2 result

    else
        result


{-| isZero
Returns true if the given string parses to 0.
Note: If the string cannot be parsed, it returns true. This is likely bad, but it has not been a issue yet.
-}
isZero : String -> Bool
isZero operand =
    operand |> String.toFloat |> Maybe.withDefault 0 |> (==) 0


{-| takesPrecedence
Whether or not a given operator has greater or equal precedence to another
-}
takesPrecedence : OperatorType -> OperatorType -> Bool
takesPrecedence op1 op2 =
    precedence op1 >= precedence op2


{-| toString
Converts given operator to a string
-}
toString : OperatorType -> String
toString operator =
    case operator of
        Add ->
            "+"

        Subtract ->
            "-"

        Divide ->
            "÷"

        Multiply ->
            "*"
