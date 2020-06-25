module Mutator exposing (Mutator(..), mutate, toString)

import Operator exposing (OperatorType(..), calculate)


type Mutator
    = Negate
    | AppendDecimalPoint
    | Percentile


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


toString : Mutator -> String
toString mutator =
    case mutator of
        Negate ->
            "+/-"

        AppendDecimalPoint ->
            "."

        Percentile ->
            "%"
