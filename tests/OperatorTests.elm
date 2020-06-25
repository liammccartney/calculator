module OperatorTests exposing (..)

import Expect
import Operator exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Operator Module"
        [ describe "calculate"
            [ test
                "It performs addition"
                (\_ ->
                    calculate Add "4" "5" |> Expect.equal "9"
                )
            , test "It performs substraction"
                (\_ ->
                    calculate Subtract "4" "5" |> Expect.equal "-1"
                )
            , test "It performs multiplication"
                (\_ ->
                    calculate Multiply "4" "5" |> Expect.equal "20"
                )
            , test "It performs decimal division"
                (\_ ->
                    calculate Divide "4" "5" |> Expect.equal "0.8"
                )
            ]
        , describe "findPrecedentOperators"
            [ test "It returns all the operators from a stack that take precedence over a given operator"
                (\_ ->
                    findPrecedentOperators [ Multiply, Divide, Add, Subtract ] Divide
                        |> Expect.equal [ Multiply, Divide ]
                )
            ]
        , describe "findLesserOperators"
            [ test "It returns all the operators from a stack that do not takes precedence over a given operator"
                (\_ ->
                    findLesserOperators [ Multiply, Divide, Subtract, Add ] Divide
                        |> Expect.equal [ Subtract, Add ]
                )
            ]
        ]
