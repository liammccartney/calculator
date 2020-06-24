module ShuntingYardTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Operator exposing (..)
import RPNExpression as RPN
import ShuntingYard as SY
import Test exposing (..)


suite : Test
suite =
    describe "The ShuntingYard Module"
        [ describe "appendOperand"
            [ test
                "It appends an operand to its expression"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "5"
                        |> SY.appendOperand "66"
                        |> SY.extractExpression
                        |> RPN.toString
                        |> Expect.equal "5 66"
                )
            ]
        , describe "appendOperator"
            [ test "It appends an operator to an empty stack"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Add
                        |> SY.extractOperatorStack
                        |> Expect.equal [ Add ]
                )
            , test "It appends higher precedence operators to a stack if the first operator is of lesser precedence"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Add
                        |> SY.appendOperand "5"
                        |> SY.appendOperator Multiply
                        |> SY.extractOperatorStack
                        |> Expect.equal [ Multiply, Add ]
                )
            , test "It removes higher precedence operators from the stack"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Add
                        |> SY.appendOperand "5"
                        |> SY.appendOperator Multiply
                        |> SY.appendOperand "3"
                        |> SY.appendOperator Divide
                        |> SY.extractOperatorStack
                        |> Expect.equal [ Divide, Add ]
                )
            , test "It shifts higher precedence operators from the stack to the expression"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Add
                        |> SY.appendOperand "5"
                        |> SY.appendOperator Multiply
                        |> SY.appendOperand "3"
                        |> SY.appendOperator Divide
                        |> SY.extractExpression
                        |> RPN.toString
                        |> Expect.equal "4 5 3 *"
                )
            ]
        , describe "replaceCurrentOperator"
            [ test "It replaces the head operator of stack"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Add
                        |> SY.replaceCurrentOperator Subtract
                        |> SY.extractOperatorStack
                        |> Expect.equal [ Subtract ]
                )
            , test "It inserts an operator to an empty stack"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "4"
                        |> SY.replaceCurrentOperator Subtract
                        |> SY.extractOperatorStack
                        |> Expect.equal [ Subtract ]
                )
            ]
        , describe "evaluate"
            [ test "It evaluates the underlying RPN Expression"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Multiply
                        |> SY.appendOperand "9"
                        |> SY.evaluate
                        |> Result.withDefault "0"
                        |> Expect.equal "36"
                )
            ]
        , describe "preemptiveEvaluate"
            [ test "It attempts to evaluate the last 3 tokens of the expression"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "3"
                        |> SY.appendOperator Add
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Multiply
                        |> SY.appendOperand "2"
                        |> SY.appendOperator Divide
                        |> SY.preemptiveEvaluate
                        |> Result.withDefault "0"
                        |> Expect.equal "8"
                )
            , test "It fails gracefully for invalid expression"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "3"
                        |> SY.appendOperator Add
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Multiply
                        |> SY.appendOperand "2"
                        |> SY.preemptiveEvaluate
                        |> Expect.equal (Err "Evaluation Failure: Too Few Operators")
                )
            ]
        , describe "currentOperand"
            [ test "It returns the current working operand of the underlying RPN expression"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "3"
                        |> SY.appendOperator Add
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Multiply
                        |> SY.appendOperand "2"
                        |> SY.currentOperand
                        |> Expect.equal "2"
                )
            , test "It doesn't get tripped up on operators"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "3"
                        |> SY.appendOperator Add
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Multiply
                        |> SY.currentOperand
                        |> Expect.equal "4"
                )
            , test "It defaults to 0"
                (\_ ->
                    SY.init
                        |> SY.currentOperand
                        |> Expect.equal "0"
                )
            ]
        , describe "currentOperator"
            [ test "It returns the current working operator of the underlying RPN expression"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "3"
                        |> SY.appendOperator Add
                        |> SY.appendOperand "4"
                        |> SY.appendOperator Multiply
                        |> SY.appendOperand "2"
                        |> SY.currentOperator
                        |> Expect.equal (Just Multiply)
                )
            , test "It returns Nothing if there are no operators"
                (\_ ->
                    SY.init
                        |> SY.appendOperand "3"
                        |> SY.currentOperator
                        |> Expect.equal Nothing
                )
            ]
        ]
