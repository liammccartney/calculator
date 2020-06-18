module Main exposing
    ( Model(..)
    , Msg(..)
    , Operation
    , Operator(..)
    , evaluate
    , incrementOperand
    , init
    , main
    , update
    , view
    )

import Browser
import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view >> Html.toUnstyled }



-- MODEL


type Operator
    = Divide
    | Multiply
    | Subtract
    | Add
    | Equals


type Mutator
    = Negate


type alias Operation =
    ( Operator, String, String )


type Model
    = LeftHandSide String
    | AwaitingRightHandSide Operator String
    | ReadyToEvaluate Operation
    | Evaluated Operation String


init : Model
init =
    LeftHandSide "0"



-- UPDATE


type Msg
    = OperandPressed String
    | OperatorPressed Operator
    | MutatorPressed Mutator
    | AllClearPressed
    | ClearPressed


update : Msg -> Model -> Model
update msg model =
    case msg of
        OperandPressed operand ->
            case model of
                LeftHandSide left ->
                    LeftHandSide (incrementOperand left operand)

                AwaitingRightHandSide operator left ->
                    ReadyToEvaluate ( operator, left, operand )

                ReadyToEvaluate ( operator, left, right ) ->
                    ReadyToEvaluate ( operator, left, incrementOperand right operand )

                Evaluated _ _ ->
                    LeftHandSide operand

        OperatorPressed Equals ->
            case model of
                LeftHandSide _ ->
                    model

                AwaitingRightHandSide operator left ->
                    let
                        operation =
                            ( operator, left, left )
                    in
                    Evaluated operation (evaluate operation)

                ReadyToEvaluate operation ->
                    Evaluated operation (evaluate operation)

                Evaluated ( operator, _, right ) result ->
                    let
                        newOperation =
                            ( operator, result, right )
                    in
                    Evaluated newOperation (evaluate newOperation)

        OperatorPressed newOperator ->
            case model of
                LeftHandSide left ->
                    AwaitingRightHandSide newOperator left

                AwaitingRightHandSide _ left ->
                    AwaitingRightHandSide newOperator left

                ReadyToEvaluate operation ->
                    AwaitingRightHandSide newOperator (evaluate operation)

                Evaluated _ result ->
                    AwaitingRightHandSide newOperator result

        MutatorPressed mutator ->
            case model of
                LeftHandSide left ->
                    LeftHandSide (mutate mutator left)

                AwaitingRightHandSide operator left ->
                    AwaitingRightHandSide operator (mutate mutator left)

                ReadyToEvaluate ( operator, left, right ) ->
                    ReadyToEvaluate ( operator, left, mutate mutator right )

                Evaluated operation result ->
                    Evaluated operation (mutate mutator result)

        AllClearPressed ->
            init

        ClearPressed ->
            case model of
                LeftHandSide _ ->
                    init

                AwaitingRightHandSide operator left ->
                    ReadyToEvaluate ( operator, left, "0" )

                ReadyToEvaluate ( operator, left, _ ) ->
                    ReadyToEvaluate ( operator, left, "0" )

                Evaluated ( operator, _, right ) _ ->
                    ReadyToEvaluate ( operator, right, "0" )


evaluate : Operation -> String
evaluate ( operator, left, right ) =
    let
        lhs =
            String.toFloat left |> Maybe.withDefault 0

        rhs =
            String.toFloat right |> Maybe.withDefault 0

        result =
            case operator of
                Add ->
                    lhs + rhs

                Multiply ->
                    lhs * rhs

                Subtract ->
                    lhs - rhs

                Divide ->
                    lhs / rhs

                Equals ->
                    rhs
    in
    result |> String.fromFloat


mutate : Mutator -> String -> String
mutate mutator operand =
    case mutator of
        Negate ->
            "-" ++ operand


incrementOperand : String -> String -> String
incrementOperand current new =
    if current == "0" then
        new

    else
        let
            newValString =
                current ++ new
        in
        if String.length newValString > 16 then
            current |> String.slice 0 16

        else
            newValString



-- VIEW


allOperands : List String
allOperands =
    [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Html.css displayTotalCss ]
            [ case model of
                LeftHandSide left ->
                    Html.text left

                AwaitingRightHandSide _ left ->
                    Html.text left

                ReadyToEvaluate ( _, _, right ) ->
                    Html.text right

                Evaluated _ result ->
                    Html.text result
            ]
        , List.append (List.map cardView allOperands)
            [ cardViewOperator Add "+"
            , cardViewOperator Subtract "-"
            , cardViewOperator Multiply "X"
            , cardViewOperator Divide "÷"
            , cardViewOperator Equals "="
            , cardViewMutator Negate "+/-"
            , clear model
            ]
            |> Html.Styled.Keyed.node "div"
                [ Html.css css ]
        ]


grey : Css.Color
grey =
    Css.hex "d8dee9"


displayTotalCss : List Css.Style
displayTotalCss =
    [ Css.width (Css.px 400)
    , Css.margin2 Css.zero Css.auto
    , Css.fontSize (Css.px 48)
    , Css.textAlign Css.right
    , Css.paddingRight (Css.px 60)
    , Css.paddingTop (Css.px 60)
    ]


cardView : String -> ( String, Html Msg )
cardView operand =
    ( "card" ++ operand
    , Html.div
        [ onClick (OperandPressed operand) ]
        [ Html.text operand ]
    )


isZero : String -> Bool
isZero operand =
    operand |> String.toFloat |> Maybe.withDefault 0 |> (==) 0


clear : Model -> ( String, Html Msg )
clear model =
    case model of
        LeftHandSide left ->
            if isZero left then
                ( "card clear"
                , Html.div
                    [ onClick AllClearPressed ]
                    [ Html.text "AC" ]
                )

            else
                ( "card clear"
                , Html.div
                    [ onClick ClearPressed ]
                    [ Html.text "C" ]
                )

        AwaitingRightHandSide _ left ->
            if isZero left then
                ( "card clear"
                , Html.div
                    [ onClick AllClearPressed ]
                    [ Html.text "AC" ]
                )

            else
                ( "card clear"
                , Html.div
                    [ onClick ClearPressed ]
                    [ Html.text "C" ]
                )

        ReadyToEvaluate ( _, _, right ) ->
            if isZero right then
                ( "card clear"
                , Html.div
                    [ onClick AllClearPressed ]
                    [ Html.text "AC" ]
                )

            else
                ( "card clear"
                , Html.div
                    [ onClick ClearPressed ]
                    [ Html.text "C" ]
                )

        Evaluated ( _, _, right ) _ ->
            if isZero right then
                ( "card clear"
                , Html.div
                    [ onClick AllClearPressed ]
                    [ Html.text "AC" ]
                )

            else
                ( "card clear"
                , Html.div
                    [ onClick ClearPressed ]
                    [ Html.text "C" ]
                )


cardViewOperator : Operator -> String -> ( String, Html Msg )
cardViewOperator operator symbol =
    ( "card" ++ symbol
    , Html.div
        [ onClick (OperatorPressed operator) ]
        [ Html.text symbol ]
    )


cardViewMutator : Mutator -> String -> ( String, Html Msg )
cardViewMutator mutator symbol =
    ( "card" ++ symbol
    , Html.div
        [ onClick (MutatorPressed mutator) ]
        [ Html.text symbol ]
    )



-- STYLES


css : List Css.Style
css =
    [ Css.displayFlex
    , Css.flexWrap Css.wrap
    , Css.margin2 (Css.px 0) Css.auto
    , Css.width <| Css.px 400
    , Css.paddingTop <| Css.rem 1
    , Css.Global.descendants
        [ Css.Global.selector "> div"
            [ Css.flexBasis <| Css.pct 20
            , Css.padding2 Css.zero (Css.px 8)
            , Css.height (Css.px 50)
            , Css.textAlign Css.center
            , Css.fontSize (Css.px 24)
            , Css.margin (Css.rem 1)
            , Css.backgroundColor grey
            ]
        , Css.Global.selector "> div > div"
            [ Css.width <| Css.pct 100
            ]
        , Css.Global.selector ".zero"
            [ Css.flexGrow (Css.int 1)
            ]
        ]
    ]
