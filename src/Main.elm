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
    ( Operator, Float, Float )


type Model
    = LeftHandSide Float
    | AwaitingRightHandSide Operator Float
    | ReadyToEvaluate Operation
    | Evaluated Operation Float


init : Model
init =
    LeftHandSide 0



-- UPDATE


type Msg
    = OperandPressed Float
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
                    ReadyToEvaluate ( operator, left, 0 )

                ReadyToEvaluate ( operator, left, _ ) ->
                    ReadyToEvaluate ( operator, left, 0 )

                Evaluated ( operator, _, right ) _ ->
                    ReadyToEvaluate ( operator, right, 0 )


evaluate : Operation -> Float
evaluate ( operator, lhs, rhs ) =
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


mutate : Mutator -> Float -> Float
mutate mutator operand =
    case mutator of
        Negate ->
            -operand


incrementOperand : Float -> Float -> Float
incrementOperand current new =
    if current == 0 then
        new

    else
        let
            newValString =
                String.fromFloat current ++ String.fromFloat new

            stringToConvert =
                if String.length newValString > 16 then
                    current |> String.fromFloat |> String.slice 0 16

                else
                    newValString
        in
        case String.toFloat stringToConvert of
            Just newVal ->
                newVal

            Nothing ->
                -- Placeholder, should handle gracefully
                0



-- VIEW


allOperands : List Float
allOperands =
    [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 0.0 ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Html.css displayTotalCss ]
            [ case model of
                LeftHandSide left ->
                    Html.text (String.fromFloat left)

                AwaitingRightHandSide _ left ->
                    Html.text (String.fromFloat left)

                ReadyToEvaluate ( _, _, right ) ->
                    Html.text (String.fromFloat right)

                Evaluated _ result ->
                    Html.text (String.fromFloat result)
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


cardView : Float -> ( String, Html Msg )
cardView operand =
    ( "card" ++ String.fromFloat operand
    , Html.div
        [ onClick (OperandPressed operand) ]
        [ Html.text (String.fromFloat operand) ]
    )


clear : Model -> ( String, Html Msg )
clear model =
    case model of
        LeftHandSide left ->
            if truncate left == 0 then
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
            if truncate left == 0 then
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
            if truncate right == 0 then
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
            if truncate right == 0 then
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
