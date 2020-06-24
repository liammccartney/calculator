module MutatorTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Mutator exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Operator Module"
        [ describe "mutate Negate"
            [ test
                "It negates a given operand"
                (\_ ->
                    mutate Negate "4" |> Expect.equal "-4"
                )
            , test "Double negation cancels the initial negation"
                (\_ ->
                    "4"
                        |> mutate Negate
                        |> mutate Negate
                        |> Expect.equal "4"
                )
            ]
        , describe "mutate AppendDecimalPoint"
            [ test "It appends a decimal point to the operand"
                (\_ ->
                    mutate AppendDecimalPoint "4" |> Expect.equal "4."
                )
            , test "It does not append a decimal point if there already is one"
                (\_ ->
                    "4" |> mutate AppendDecimalPoint |> mutate AppendDecimalPoint |> Expect.equal "4."
                )
            ]
        , describe "mutate Percentile"
            [ test "Divides the given operand by 100"
                (\_ ->
                    "400" |> mutate Percentile |> Expect.equal "4"
                )
            ]
        ]
