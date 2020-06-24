module MainTests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Main exposing (..)
import Operator exposing (..)
import RPNExpression as RPN
import ShuntingYard
import Test exposing (..)


suite : Test
suite =
    describe "The Main Module"
        [ describe "incrementOperand"
            [ test "Increments an integer value" (\_ -> incrementOperand "1" "1" |> Expect.equal "11")
            , test "Increments from 0" (\_ -> incrementOperand "0" "4" |> Expect.equal "4")
            , test "Constrains to 16 digits long" (\_ -> incrementOperand "123456789123456789" "1" |> Expect.equal "1234567891234567")
            , test "Constrains to 16 digits long favoring current operand" (\_ -> incrementOperand "1" "123456789123456789" |> Expect.equal "1")
            ]
        , describe "update"
            [ test "Inputting left hand side"
                (\_ ->
                    init
                        |> update (OperandPressed "1")
                        |> update (OperandPressed "5")
                        |> (\{ input } -> input)
                        |> Expect.equal (InputLeft "15")
                )
            , test "Inputting an operator after a left hand side"
                (\_ ->
                    init
                        |> update (OperandPressed "1")
                        |> update (OperandPressed "5")
                        |> update (OperatorPressed Divide)
                        |> (\{ input } -> input)
                        |> Expect.equal (NeedRight "15")
                )
            , test "Inputting an operand after an operator"
                (\_ ->
                    init
                        |> update (OperandPressed "1")
                        |> update (OperandPressed "5")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "5")
                        |> (\{ input } -> input)
                        |> Expect.equal (InputRight "5")
                )
            , test "Evaluating an expression"
                (\_ ->
                    init
                        |> update (OperandPressed "1")
                        |> update (OperandPressed "5")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "5")
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "3")
                )
            , test "Repeating an evaluation"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "8")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> update EqualsPressed
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "6")
                )
            , test "Evaluating a new expression after evaluation"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "8")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Multiply)
                        |> update (OperandPressed "8")
                        |> update (OperandPressed "8")
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "176")
                )
            , test "Evaluating an expression without entering a left hand side operand"
                (\_ ->
                    init
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "2")
                        |> update (OperandPressed "2")
                        |> update (OperandPressed "8")
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "228")
                )
            , test "Changing the current operator"
                (\_ ->
                    init
                        |> update (OperandPressed "1")
                        |> update (OperandPressed "6")
                        |> update (OperatorPressed Add)
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "8")
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "2")
                )
            , test "Chaining operations"
                (\_ ->
                    init
                        |> update (OperandPressed "3")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Multiply)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "3")
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "10")
                )
            , test "Reusing the right hand side of a previous operation"
                (\_ ->
                    init
                        |> update (OperandPressed "4")
                        |> update (OperandPressed "8")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "2")
                        |> update EqualsPressed
                        |> update (OperandPressed "8")
                        |> update (OperandPressed "8")
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "44")
                )
            , test "Using the result of an operation as the left hand side of a new operation"
                (\_ ->
                    init
                        |> update (OperandPressed "5")
                        |> update (OperandPressed "5")
                        |> update (OperatorPressed Divide)
                        |> update (OperandPressed "5")
                        |> update EqualsPressed
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "3")
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "14")
                )
            , test "Changing operator after eager evaluation"
                (\_ ->
                    init
                        |> update (OperandPressed "3")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Multiply)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Divide)
                        |> update (OperatorPressed Multiply)
                        |> update (OperandPressed "6")
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "51")
                )
            , test "Evaluating an empty input"
                (\_ ->
                    init
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "0")
                )
            , test "Evaluation Shortcut"
                (\_ ->
                    init
                        |> update (OperandPressed "3")
                        |> update (OperatorPressed Add)
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "6")
                )
            , test "Evaluating after eager evaluation"
                (\_ ->
                    init
                        |> update (OperandPressed "3")
                        |> update (OperatorPressed Add)
                        |> update (OperandPressed "4")
                        |> update (OperatorPressed Multiply)
                        |> update (OperandPressed "2")
                        |> update (OperatorPressed Divide)
                        |> update EqualsPressed
                        |> (\{ input } -> input)
                        |> Expect.equal (Evaluated "8.375")
                )
            ]
        ]
