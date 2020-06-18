module MainTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Main
import Test exposing (..)


extractLeftHandSide : Main.Model -> String
extractLeftHandSide model =
    case model of
        Main.LeftHandSide left ->
            left

        Main.AwaitingRightHandSide _ left ->
            left

        Main.ReadyToEvaluate ( _, left, _ ) ->
            left

        Main.Evaluated ( _, left, _ ) _ ->
            left


extractRightHandSide : Main.Model -> Maybe String
extractRightHandSide model =
    case model of
        Main.LeftHandSide left ->
            Nothing

        Main.AwaitingRightHandSide _ left ->
            Nothing

        Main.ReadyToEvaluate ( _, _, right ) ->
            Just right

        Main.Evaluated ( _, _, right ) _ ->
            Just right


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
        [ describe "Main.evaluate"
            [ test "Add two numbers" (\_ -> Expect.equal "4" (( Main.Add, "2", "2" ) |> Main.evaluate))
            , test "Multiply two numbers" (\_ -> Expect.equal "4" (( Main.Multiply, "2", "2" ) |> Main.evaluate))
            , test "Subtract two numbers" (\_ -> Expect.equal "0" (( Main.Subtract, "2", "2" ) |> Main.evaluate))
            , test "Subtracting two numbers can be negative" (\_ -> Expect.equal "-5" (( Main.Subtract, "0", "5" ) |> Main.evaluate))
            , test "Divide two numbers" (\_ -> Expect.equal "1" (( Main.Divide, "2", "2" ) |> Main.evaluate))
            , test "Divide by zero" (\_ -> Expect.true "dividing by zero to be infinite" (( Main.Divide, "2", "0" ) |> Main.evaluate |> (==) "Infinity"))
            ]
        , describe "Main.incrementOperand"
            [ test "Increments an integer value" (\_ -> Main.incrementOperand "1" "1" |> Expect.equal "11")
            , test "Increments a decimal value" (\_ -> Main.incrementOperand "1.1" "1" |> Expect.equal "1.11")
            , test "Constrains to 16 digits long" (\_ -> Main.incrementOperand "123456789123456789" "1" |> Expect.equal "1234567891234567")
            , test "Constrains to 16 digits long favoring current operand" (\_ -> Main.incrementOperand "1" "123456789123456789" |> Expect.equal "1")
            ]
        , describe "Main.mutate Negate"
            [ test "Changes a positive operand to negative" (\_ -> Main.mutate Main.Negate "1" |> Expect.equal "-1")
            , test "Changes a negative operand to positive" (\_ -> Main.mutate Main.Negate "-1" |> Expect.equal "1")
            , test "Does not negate zero" (\_ -> Main.mutate Main.Negate "0" |> Expect.equal "0")
            ]
        , describe "Main.mutate AppendDecimalPoint"
            [ test "Adds a decimal point to the operand" (\_ -> Main.mutate Main.AppendDecimalPoint "0" |> Expect.equal "0.")
            , test "Does not add a decimal point if there already is one" (\_ -> Main.mutate Main.AppendDecimalPoint "1.2" |> Expect.equal "1.2")
            ]
        , describe "Main.update"
            [ fuzz float
                "Increasing left hand side from 0"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
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
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed "2")
                        |> extractLeftHandSide
                        |> Expect.equal (Main.incrementOperand operand "2")
                )
            , fuzz float
                "Negating the left hand side"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.MutatorPressed Main.Negate)
                        |> extractLeftHandSide
                        |> Expect.equal (Main.mutate Main.Negate operand)
                )
            , fuzz int
                "Appending a decimal point to the lefthand side"
                (\integer ->
                    let
                        operand =
                            String.fromInt integer
                    in
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.MutatorPressed Main.AppendDecimalPoint)
                        |> extractLeftHandSide
                        |> Expect.equal (Main.mutate Main.AppendDecimalPoint operand)
                )
            , fuzz float
                "Adding an operator"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Expect.equal (Main.AwaitingRightHandSide Main.Add (Main.incrementOperand operand "2"))
                )
            , fuzz float
                "Inputting a right hand side"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed "2")
                        |> Expect.equal (Main.ReadyToEvaluate ( Main.Add, Main.incrementOperand operand "2", "2" ))
                )
            , fuzz float
                "Negating the right hand side"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.MutatorPressed Main.Negate)
                        |> extractRightHandSide
                        |> Maybe.withDefault "0"
                        |> Expect.equal (Main.mutate Main.Negate "2")
                )
            , fuzz int
                "Appending a decimal point to the right hand side"
                (\integer ->
                    let
                        operand =
                            String.fromInt integer
                    in
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.MutatorPressed Main.AppendDecimalPoint)
                        |> extractRightHandSide
                        |> Maybe.withDefault "0"
                        |> Expect.equal (Main.mutate Main.AppendDecimalPoint "2")
                )
            , fuzz float
                "Evaluating an operation"
                (\float ->
                    let
                        operand =
                            String.fromFloat float
                    in
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.Evaluated ( Main.Add, Main.incrementOperand operand "2", "2" ) (Main.evaluate ( Main.Add, Main.incrementOperand operand "2", "2" )))
                )
            , test
                "Repeating an evaluation"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.Evaluated ( Main.Divide, "22", "2" ) (Main.evaluate ( Main.Divide, "22", "2" )))
                )
            , test
                "Starting a new operation from a unevaluated one"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Expect.equal (Main.AwaitingRightHandSide Main.Add "22")
                )
            , test
                "Starting and then completing a new operation from a unevaluated one"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed "5")
                        |> Expect.equal (Main.ReadyToEvaluate ( Main.Add, "22", "5" ))
                )
            , test
                "Evaluating an incomplete operation, just left hand side"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.LeftHandSide "44")
                )
            , test
                "Shortcut to evaluation where left and right sides are the same"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Multiply)
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.Evaluated ( Main.Multiply, "44", "44" ) (Main.evaluate ( Main.Multiply, "44", "44" )))
                )
            , test
                "Negating the result of an evaluated operation"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Main.update (Main.MutatorPressed Main.Negate)
                        |> Expect.equal (Main.Evaluated ( Main.Divide, "44", "2" ) "-22")
                )
            , test
                "Evaluating a operation using the negated result of a previous operation"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Main.update (Main.MutatorPressed Main.Negate)
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed "5")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.Evaluated ( Main.Add, "-22", "5" ) "-17")
                )
            , test
                "Appending a decimal point after evaluating an operation starts a new operation"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Main.update (Main.MutatorPressed Main.AppendDecimalPoint)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperandPressed "2")
                        |> Expect.equal (Main.LeftHandSide "0.22")
                )
            , test
                "Evaluating a new operation after starting a new one with appending a decimal point"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperandPressed "4")
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Main.update (Main.MutatorPressed Main.AppendDecimalPoint)
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperandPressed "2")
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed "5")
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.Evaluated ( Main.Add, "0.22", "5" ) "5.22")
                )
            , test
                "A user cannot create an operand longer than 16 digits"
                (\_ ->
                    Main.init
                        |> executeNTimes 20 (Main.update (Main.OperandPressed "4"))
                        |> Expect.equal (Main.LeftHandSide "4444444444444444")
                )
            ]
        ]
