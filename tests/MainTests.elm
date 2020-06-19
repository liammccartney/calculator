module MainTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Main exposing (..)
import Test exposing (..)


extractLeftHandSide : Model -> String
extractLeftHandSide model =
    case model of
        LeftHandSide left ->
            left

        AwaitingRightHandSide _ left ->
            left

        ReadyToEvaluate ( _, left, _ ) ->
            left

        Evaluated ( _, left, _ ) _ ->
            left


extractRightHandSide : Model -> String
extractRightHandSide model =
    let
        rhs =
            case model of
                LeftHandSide left ->
                    Nothing

                AwaitingRightHandSide _ left ->
                    Nothing

                ReadyToEvaluate ( _, _, right ) ->
                    Just right

                Evaluated ( _, _, right ) _ ->
                    Just right
    in
    Maybe.withDefault "0" rhs


extractResult : Model -> String
extractResult model =
    let
        rhs =
            case model of
                LeftHandSide _ ->
                    Nothing

                AwaitingRightHandSide _ _ ->
                    Nothing

                ReadyToEvaluate _ ->
                    Nothing

                Evaluated ( _, _, _ ) result ->
                    Just result
    in
    Maybe.withDefault "0" rhs


executeNTimes : Int -> (a -> a) -> a -> a
executeNTimes n func arg =
    case n of
        1 ->
            func arg

        _ ->
            executeNTimes (n - 1) func (func arg)


suite : Test
suite =
    describe "The Main Module"
        [ describe "evaluate"
            [ test "Add two numbers" (\_ -> Expect.equal "4" (( Add, "2", "2" ) |> evaluate))
            , test "Multiply two numbers" (\_ -> Expect.equal "4" (( Multiply, "2", "2" ) |> evaluate))
            , test "Subtract two numbers" (\_ -> Expect.equal "0" (( Subtract, "2", "2" ) |> evaluate))
            , test "Subtracting two numbers can be negative" (\_ -> Expect.equal "-5" (( Subtract, "0", "5" ) |> evaluate))
            , test "Divide two numbers" (\_ -> Expect.equal "1" (( Divide, "2", "2" ) |> evaluate))
            , test "Divide by zero" (\_ -> Expect.true "dividing by zero to be infinite" (( Divide, "2", "0" ) |> evaluate |> (==) "Infinity"))
            ]
        , describe "incrementOperand"
            [ test "Increments an integer value" (\_ -> incrementOperand "1" "1" |> Expect.equal "11")
            , test "Increments a decimal value" (\_ -> incrementOperand "1.1" "1" |> Expect.equal "1.11")
            , test "Constrains to 16 digits long" (\_ -> incrementOperand "123456789123456789" "1" |> Expect.equal "1234567891234567")
            , test "Constrains to 16 digits long favoring current operand" (\_ -> incrementOperand "1" "123456789123456789" |> Expect.equal "1")
            ]
        , describe "mutate Negate"
            [ test "Changes a positive operand to negative" (\_ -> mutate Negate "1" |> Expect.equal "-1")
            , test "Changes a negative operand to positive" (\_ -> mutate Negate "-1" |> Expect.equal "1")
            , test "Does not negate zero" (\_ -> mutate Negate "0" |> Expect.equal "0")
            ]
        , describe "mutate AppendDecimalPoint"
            [ test "Adds a decimal point to the operand" (\_ -> mutate AppendDecimalPoint "0" |> Expect.equal "0.")
            , test "Does not add a decimal point if there already is one" (\_ -> mutate AppendDecimalPoint "1.2" |> Expect.equal "1.2")
            ]
        , describe "mutate Percentile"
            [ fuzz float
                "Divides an operand by 100"
                (\float ->
                    float
                        |> String.fromFloat
                        |> mutate Percentile
                        |> Expect.equal (100 |> (/) float |> String.fromFloat)
                )
            ]
        , describe "update"
            [ fuzz float
                "Increasing left hand side from 0"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    init
                        |> update (OperandPressed operand)
                        |> extractLeftHandSide
                        |> Expect.equal operand
                )
            , fuzz float
                "Increasing left hand side from non zero"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    init
                        |> update (OperandPressed operand)
                        |> update (OperandPressed "2")
                        |> extractLeftHandSide
                        |> Expect.equal (incrementOperand operand "2")
                )
            , fuzz float
                "Negating the left hand side"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    init
                        |> update (OperandPressed operand)
                        |> update (MutatorPressed Negate)
                        |> extractLeftHandSide
                        |> Expect.equal (mutate Negate operand)
                )
            , fuzz int
                "Appending a decimal point to the lefthand side"
                (\integer ->
                    let
                        operand =
                            String.fromInt integer
                    in
                    init
                        |> update (OperandPressed operand)
                        |> update (MutatorPressed AppendDecimalPoint)
                        |> extractLeftHandSide
                        |> Expect.equal (mutate AppendDecimalPoint operand)
                )
            , fuzz float
                "Adding an operator"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    init
                        |> update (OperandPressed operand)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> Expect.equal (AwaitingRightHandSide Add (incrementOperand operand "2"))
                )
            , fuzz float
                "Inputting a right hand side"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    init
                        |> update (OperandPressed operand)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "2")
                        |> Expect.equal (ReadyToEvaluate ( Add, incrementOperand operand "2", "2" ))
                )
            , fuzz float
                "Negating the right hand side"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    init
                        |> update (OperandPressed operand)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "2")
                        |> update (MutatorPressed Negate)
                        |> extractRightHandSide
                        |> Expect.equal (mutate Negate "2")
                )
            , fuzz int
                "Appending a decimal point to the right hand side"
                (\integer ->
                    let
                        operand =
                            String.fromInt integer
                    in
                    init
                        |> update (OperandPressed operand)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "2")
                        |> update (MutatorPressed AppendDecimalPoint)
                        |> extractRightHandSide
                        |> Expect.equal (mutate AppendDecimalPoint "2")
                )
            , test
                "Percentiling the left hand side"
                (\_ ->
                    init
                        |> update (OperandPressed "2")
                        |> update (OperandPressed "2")
                        |> update (MutatorPressed Percentile)
                        |> extractLeftHandSide
                        |> Expect.equal "0.22"
                )
            , test
                "Percentiling the right hand side"
                (\_ ->
                    init
                        |> update (OperandPressed "2")
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Multiply)
                        |> update (OperandPressed "9")
                        |> update (OperandPressed "0")
                        |> update (OperandPressed "0")
                        |> update (MutatorPressed Percentile)
                        |> extractRightHandSide
                        |> Expect.equal "9"
                )
            , fuzz float
                "Evaluating an operation"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    init
                        |> update (OperandPressed operand)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> Expect.equal (Evaluated ( Add, incrementOperand operand "2", "2" ) (evaluate ( Add, incrementOperand operand "2", "2" )))
                )
            , test
                "Repeating an evaluation"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> update EqualsPressed
                        |> Expect.equal (Evaluated ( Divide, "22", "2" ) (evaluate ( Divide, "22", "2" )))
                )
            , test
                "Starting a new operation from a unevaluated one"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> Expect.equal (AwaitingRightHandSide Add "22")
                )
            , test
                "Starting and then completing a new operation from a unevaluated one"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "5")
                        |> Expect.equal (ReadyToEvaluate ( Add, "22", "5" ))
                )
            , test
                "Evaluating an incomplete operation, just left hand side"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update EqualsPressed
                        |> Expect.equal (LeftHandSide "44")
                )
            , test
                "Shortcut to evaluation where left and right sides are the same"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Multiply)
                        |> update EqualsPressed
                        |> Expect.equal (Evaluated ( Multiply, "44", "44" ) (evaluate ( Multiply, "44", "44" )))
                )
            , test
                "Negating the result of an evaluated operation"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> update (MutatorPressed Negate)
                        |> Expect.equal (Evaluated ( Divide, "44", "2" ) "-22")
                )
            , test
                "Evaluating a operation using the negated result of a previous operation"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> update (MutatorPressed Negate)
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "5")
                        |> update EqualsPressed
                        |> Expect.equal (Evaluated ( Add, "-22", "5" ) "-17")
                )
            , test
                "Appending a decimal point after evaluating an operation starts a new operation"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> update (MutatorPressed AppendDecimalPoint)
                        |> update (OperandPressed "2")
                        |> update (OperandPressed "2")
                        |> Expect.equal (LeftHandSide "0.22")
                )
            , test
                "Evaluating a new operation after starting a new one with appending a decimal point"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> update (MutatorPressed AppendDecimalPoint)
                        |> update (OperandPressed "2")
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "5")
                        |> update EqualsPressed
                        |> Expect.equal (Evaluated ( Add, "0.22", "5" ) "5.22")
                )
            , test
                "Percentiling the result of an evaluated operation"
                (\_ ->
                    init
                        |> update (OperandPressed "2")
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Multiply)
                        |> update (OperandPressed "9")
                        |> update (OperandPressed "0")
                        |> update (OperandPressed "0")
                        |> update EqualsPressed
                        |> update (MutatorPressed Percentile)
                        |> extractResult
                        |> Expect.equal "198"
                )
            , test
                "A user cannot create an operand longer than 16 digits"
                (\_ ->
                    init
                        |> executeNTimes 20 (update (OperandPressed "4"))
                        |> Expect.equal (LeftHandSide "4444444444444444")
                )
            ]
        ]
