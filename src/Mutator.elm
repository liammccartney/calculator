module Mutator exposing (..)

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
