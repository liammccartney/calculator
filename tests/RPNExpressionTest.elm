module RPNExpressionTest exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Operator exposing (..)
import RPNExpression exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The RPNExpression Module"
        [ describe "appendOperand"
            [ test
                "It appends the given operand to the end of the expression"
                (\_ ->
                    emptyExpression
                        |> appendOperand "3"
                        |> appendOperand "4"
                        |> RPNExpression.toString
                        |> Expect.equal "3 4"
                )
            ]
        , describe "appendOperator"
            [ test
                "It appends the given operator to the end of the expression"
                (\_ ->
                    emptyExpression
                        |> appendOperand "3"
                        |> appendOperand "7"
                        |> appendOperator Add
                        |> appendOperand "6"
                        |> appendOperator Subtract
                        |> appendOperand "2"
                        |> appendOperator Divide
                        |> RPNExpression.toString
                        |> Expect.equal "3 7 + 6 - 2 ÷"
                )
            ]
        , describe "currentOperand"
            [ test
                "It returns the last operand in the expression"
                (\_ ->
                    emptyExpression
                        |> appendOperand "3"
                        |> appendOperand "4"
                        |> appendOperator Add
                        |> currentOperand
                        |> Expect.equal "4"
                )
            ]
        , describe "evaluate"
            [ test "It correctly evaluates a valid RPN expression"
                (\_ ->
                    emptyExpression
                        |> appendOperand "3"
                        |> appendOperand "4"
                        |> appendOperator Add
                        |> evaluate
                        |> Result.withDefault "error"
                        |> Expect.equal "7"
                )
            , test "It correctly evaluates a more complex RPN expression"
                (\_ ->
                    emptyExpression
                        |> appendOperand "60"
                        |> appendOperand "5"
                        |> appendOperator Divide
                        |> appendOperand "19"
                        |> appendOperator Multiply
                        |> appendOperand "10"
                        |> appendOperator Add
                        |> evaluate
                        |> Result.withDefault "error"
                        |> Expect.equal "238"
                )
            , test "It returns an error for an invalid RPN expression, too few operators"
                (\_ ->
                    emptyExpression
                        |> appendOperand "60"
                        |> appendOperand "5"
                        |> appendOperator Divide
                        |> appendOperand "19"
                        |> appendOperator Multiply
                        |> appendOperand "10"
                        |> evaluate
                        |> Expect.equal (Err "Evaluation Failure")
                )
            , test "It returns an error for an invalid RPN expression, expression too short"
                (\_ ->
                    emptyExpression
                        |> appendOperand "60"
                        |> appendOperator Divide
                        |> evaluate
                        |> Expect.equal (Err "Evaluation Failure")
                )
            ]
        ]
