module Mutator exposing (Mutator(..), mutate, toString)

{-| The Mutator is a different kind of operator in this calculator.
It is intended to directly manipulate a given operand, without making adjustments to the overall expression.
There are three defined mutators
Negate --> multiplies the operand by -1
AppendDecimalPoint --> Appends a decimal point to the operand, only if there isn't one already
Percentile --> Divides the operand by 100
-}

import Operator exposing (OperatorType(..), calculate)


type Mutator
    = Negate
    | AppendDecimalPoint
    | Percentile


{-| mutate
mutate that operand for a given mutator and operand.
-}
mutate : Mutator -> String -> String
mutate mutator operand =
    case mutator of
        Negate ->
            calculate Multiply "-1" operand

        AppendDecimalPoint ->
            if String.contains "." operand then
                operand

            else
                operand ++ "."

        Percentile ->
            calculate Divide operand "100"


{-| toSring
Converts the given mutator to a string
-}
toString : Mutator -> String
toString mutator =
    case mutator of
        Negate ->
            "+/-"

        AppendDecimalPoint ->
            "."

        Percentile ->
            "%"
