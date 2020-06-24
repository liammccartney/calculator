module Operator exposing (OperatorType(..), calculate, findLesserOperators, findPrecedentOperators, toString)

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


calculate : OperatorType -> Int -> Int -> Int
calculate operator left right =
    case operator of
        Add ->
            left + right

        Subtract ->
            left - right

        Divide ->
            left // right

        Multiply ->
            left * right


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
