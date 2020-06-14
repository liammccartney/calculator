module Operand exposing (..)

import Html.Events exposing (onClick)
import Html.Styled as Html exposing (Html, div, text)


type OperandMsg
    = OperandPressed Operand


type Operand
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


view operand =
    case operand of
        Zero ->
            div [] [ text "0" ]

        One ->
            div [] [ text "1" ]

        Two ->
            div [] [ text "2" ]

        Three ->
            div [] [ text "3" ]

        Four ->
            div [] [ text "4" ]

        Five ->
            div [] [ text "5" ]

        Six ->
            div [] [ text "6" ]

        Seven ->
            div [] [ text "7" ]

        Eight ->
            div [] [ text "8" ]

        Nine ->
            div [] [ text "9" ]


allOperands =
    [ One
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Zero
    ]


toInt operand =
    case operand of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9
