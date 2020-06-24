module Operator exposing (OperatorType(..), calculate, findLesserOperators, findPrecedentOperators, toString)

import Decimal
import List.Extra


type OperatorType
    = Add
    | Subtract
    | Multiply
    | Divide


findPrecedentOperators : List OperatorType -> OperatorType -> List OperatorType
findPrecedentOperators operatorsList operator =
    List.Extra.takeWhile (\op -> takesPrecedence op operator) operatorsList


findLesserOperators : List OperatorType -> OperatorType -> List OperatorType
findLesserOperators operatorsList operator =
    List.Extra.dropWhile (\op -> takesPrecedence op operator) operatorsList


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
    if String.endsWith ".0" result then
        String.slice 0 -2 result

    else
        result


isZero : String -> Bool
isZero operand =
    operand |> String.toFloat |> Maybe.withDefault 0 |> (==) 0


takesPrecedence : OperatorType -> OperatorType -> Bool
takesPrecedence op1 op2 =
    precedence op1 >= precedence op2


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
