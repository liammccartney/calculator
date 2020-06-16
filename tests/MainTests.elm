module MainTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Main
import Test exposing (..)


extractLeftHandSide : Main.Model -> Float
extractLeftHandSide model =
    case model of
        Main.LeftHandSide left ->
            left

        Main.AwaitingRightHandSide _ left ->
            left

        Main.ReadyToEvaluate ( _, left, _ ) ->
            left

        Main.Evaluated ( _, left, _ ) ->
            left


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
            [ test "Add two numbers" (\_ -> Expect.equal 4 (( Main.Add, 2, 2 ) |> Main.evaluate))
            , test "Multiply two numbers" (\_ -> Expect.equal 4 (( Main.Multiply, 2, 2 ) |> Main.evaluate))
            , test "Subtract two numbers" (\_ -> Expect.equal 0 (( Main.Subtract, 2, 2 ) |> Main.evaluate))
            , test "Subtracting two numbers can be negative" (\_ -> Expect.equal -5 (( Main.Subtract, 0, 5 ) |> Main.evaluate))
            , test "Divide two numbers" (\_ -> Expect.equal 1 (( Main.Divide, 2, 2 ) |> Main.evaluate))
            , test "Divide by zero" (\_ -> Expect.true "dividing by zero to be infinite" (( Main.Divide, 2, 0 ) |> Main.evaluate |> isInfinite))
            ]
        , describe "Main.incrementOperand"
            [ test "Increments an integer value" (\_ -> Main.incrementOperand 1 1 |> Expect.equal 11)
            , test "Increments a decimal value" (\_ -> Main.incrementOperand 1.1 1 |> Expect.within (Absolute 0.000000001) 1.11)
            , test "Constrains to 16 digits long" (\_ -> Main.incrementOperand 123456789123456789 1 |> Expect.equal 1234567891234567)
            , test "Constrains to 16 digits long favoring current operand" (\_ -> Main.incrementOperand 1 123456789123456789 |> Expect.equal 1)
            ]
        , describe "update"
            [ fuzz float
                "Increasing left hand side from 0"
                (\operand ->
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> extractLeftHandSide
                        |> Expect.within (Absolute 0.00000001) operand
                )
            , fuzz float
                "Increasing left hand side from non zero"
                (\operand ->
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed 2)
                        |> extractLeftHandSide
                        |> Expect.within (Absolute 0.00000001) (Main.incrementOperand operand 2)
                )
            , fuzz float
                "Adding an operator"
                (\operand ->
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed 2)
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Expect.equal (Main.AwaitingRightHandSide Main.Add (Main.incrementOperand operand 2))
                )
            , fuzz float
                "Inputting a right hand side"
                (\operand ->
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed 2)
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed 2)
                        |> Expect.equal (Main.ReadyToEvaluate ( Main.Add, Main.incrementOperand operand 2, 2 ))
                )
            , fuzz float
                "Evaluating an operation"
                (\operand ->
                    Main.init
                        |> Main.update (Main.OperandPressed operand)
                        |> Main.update (Main.OperandPressed 2)
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed 2)
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.Evaluated ( Main.Add, Main.incrementOperand operand 2, 2 ))
                )
            , test
                "Repeating an evaluation"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed 2)
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.Evaluated ( Main.Divide, 22, 2 ))
                )
            , test
                "Starting a new operation from a unevaluated one"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed 2)
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Expect.equal (Main.AwaitingRightHandSide Main.Add 22)
                )
            , test
                "Starting and then completing a new operation from a unevaluated one"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperatorPressed Main.Divide)
                        |> Main.update (Main.OperandPressed 2)
                        |> Main.update (Main.OperatorPressed Main.Add)
                        |> Main.update (Main.OperandPressed 5)
                        |> Expect.equal (Main.ReadyToEvaluate ( Main.Add, 22, 5 ))
                )
            , test
                "Evaluating an incomplete operation, just left hand side"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.LeftHandSide 44)
                )
            , test
                "Shortcut to evaluation where left and right sides are the same"
                (\_ ->
                    Main.init
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperandPressed 4)
                        |> Main.update (Main.OperatorPressed Main.Multiply)
                        |> Main.update (Main.OperatorPressed Main.Equals)
                        |> Expect.equal (Main.Evaluated ( Main.Multiply, 44, 44 ))
                )
            , test
                "A user cannot create an operand longer than 16 digits"
                (\_ ->
                    Main.init
                        |> executeNTimes 20 (Main.update (Main.OperandPressed 4))
                        |> Expect.equal (Main.LeftHandSide 4444444444444444)
                )
            ]
        ]
