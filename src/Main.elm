module Main exposing (main)

import Array
import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Json.Decode as Decode
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Action
    = Forward
    | Backward


type Movement
    = Idle
    | StartMoving Action
    | Moving Action ( Int, Int )


type KeyMode
    = Advance
    | Rotate


type Orientation
    = Left
    | Right
    | Up
    | Down


type alias Model =
    { position : { x : Int, y : Int }
    , facing : Orientation
    , movement : Movement
    , keyMode : KeyMode
    }


type Msg
    = GotInput Orientation
    | GotFrame Int
    | ToggleKeyMode
    | Interrupt


init : flags -> ( Model, Cmd msg )
init _ =
    ( { position = { x = 0, y = 0 }
      , movement = Idle
      , facing = Down
      , keyMode = Advance
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ position } as model) =
    case msg of
        GotInput orientation ->
            case model.keyMode of
                Advance ->
                    ( { model
                        | movement = startMoving model Forward
                        , facing = orientation
                      }
                    , Cmd.none
                    )

                Rotate ->
                    ( rotateOrMove orientation model
                    , Cmd.none
                    )

        GotFrame time ->
            ( case model.movement of
                Idle ->
                    { model | movement = Idle }

                StartMoving action ->
                    { model
                        | movement = Moving action ( time, time )
                        , position = move model.facing action position
                    }

                Moving action ( start, _ ) ->
                    { model
                        | movement = Moving action ( start, time )
                        , position =
                            if (time - start) > 300 then
                                move model.facing action model.position

                            else
                                model.position
                    }
            , Cmd.none
            )

        ToggleKeyMode ->
            ( { model
                | keyMode =
                    case model.keyMode of
                        Advance ->
                            Rotate

                        Rotate ->
                            Advance
              }
            , Cmd.none
            )

        Interrupt ->
            ( { model | movement = Idle }
            , Cmd.none
            )


startMoving : Model -> Action -> Movement
startMoving model action =
    case model.movement of
        Idle ->
            StartMoving action

        StartMoving _ ->
            StartMoving action

        Moving _ time ->
            Moving action time


move : Orientation -> Action -> { x : Int, y : Int } -> { x : Int, y : Int }
move orientation action position =
    let
        amount =
            case action of
                Forward ->
                    1

                Backward ->
                    -1
    in
    case orientation of
        Left ->
            { position | x = constrain (position.x - amount) }

        Right ->
            { position | x = constrain (position.x + amount) }

        Up ->
            { position | y = constrain (position.y - amount) }

        Down ->
            { position | y = constrain (position.y + amount) }


constrain : Int -> Int
constrain =
    clamp 0 (gridDimensions - 1)


rotateOrMove : Orientation -> Model -> Model
rotateOrMove orientation model =
    case orientation of
        Up ->
            { model | movement = startMoving model Forward }

        Down ->
            { model | movement = startMoving model Backward }

        _ ->
            { model
                | facing =
                    turnBySteps
                        (case orientation of
                            Left ->
                                -1

                            Right ->
                                1

                            _ ->
                                0
                        )
                        model.facing
            }


turnBySteps : Int -> Orientation -> Orientation
turnBySteps steps orientation =
    let
        orientationIdx =
            orientations
                |> Array.foldl
                    (\dir ( found, idx ) ->
                        if found || dir == orientation then
                            ( True, idx )

                        else
                            ( False, idx + 1 )
                    )
                    ( False, 0 )
                |> Tuple.second
    in
    Array.get (modBy 4 (orientationIdx + steps)) orientations
        |> Maybe.withDefault orientation


orientations : Array.Array Orientation
orientations =
    Array.fromList [ Left, Up, Right, Down ]


view : Model -> Html Msg
view model =
    Html.div
        [ style "position" "relative"
        , style "width" "600px"
        , style "height" "600px"
        ]
        [ Html.div []
            (List.repeat gridDimensions
                (Html.div
                    [ style "display" "flex" ]
                    (List.repeat gridDimensions cell)
                )
            )
        , Html.div
            [ style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            ]
            [ robot model
            ]
        , Html.div
            [ style "position" "absolute"
            , style "bottom" "0"
            , style "right" "0"
            ]
            [ Html.div
                [ style "display" "flex"
                , style "align-items" "end"
                ]
                [ Html.div
                    [ style "flex" "grow" ]
                    [ cardinalButton model Left ]
                , Html.div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "flex" "grow"
                    ]
                    [ cardinalButton model Up
                    , cardinalButton model Down
                    ]
                , Html.div
                    [ style "flex" "grow" ]
                    [ cardinalButton model Right ]
                ]
            , Html.button
                [ style "width" "100%"
                , onClick ToggleKeyMode
                ]
                [ Html.text "\u{00A0}" ]
            , Html.text "Keyboard works too"
            ]
        ]


cell : Html msg
cell =
    Html.div
        [ style "box-sizing" "border-box"
        , style "height" (intToPx cellSize)
        , style "width" (intToPx cellSize)
        , style "padding" "5px"
        ]
        [ Html.div
            [ style "border-radius" "5px"
            , style "background" "lightgray"
            , style "height" "100%"
            , style "width" "100%"
            ]
            []
        ]


robot : Model -> Html msg
robot { position, facing } =
    let
        { x, y } =
            position
    in
    Html.div
        [ style "box-sizing" "border-box"
        , style "height" (intToPx cellSize)
        , style "width" (intToPx cellSize)
        , style "padding" "5px"
        , style "transition" "200ms ease-in-out"
        , style "transform"
            (String.concat
                [ "translateY(" ++ translatePixels y ++ ")"
                , "translateX(" ++ translatePixels x ++ ")"
                ]
            )
        ]
        [ Html.div
            [ style "box-sizing" "border-box"
            , style "border-radius" "5px"
            , style "background" "gray"
            , style "height" "60%"
            , style "width" "80%"
            , style "margin" "auto"
            , style "margin-top" "15%"
            , style "display" "flex"
            ]
            [ Html.div
                [ style "display" "flex"
                , style "justify-content" "space-around"
                , style "width" "100%"
                , style "height" "100%"
                ]
                (List.repeat 2
                    (Html.div
                        [ style "animation" "5.6s eyeBlink 0.35s linear infinite"
                        , style "border-radius" "60%"
                        , style "height" "40%"
                        , style "width" "20%"
                        , style "background-color" "#3498db"
                        , style "transition" "40ms ease-in-out"
                        , case facing of
                            Up ->
                                style "transform" "translateY(30%) rotate(-180deg)"

                            Right ->
                                style "transform" "translateY(60%) rotate(90deg)"

                            Left ->
                                style "transform" "translateY(60%) rotate(-90deg)"

                            Down ->
                                style "transform" "translateY(120%) rotate(180deg)"
                        ]
                        [ Html.div
                            [ style "border-radius" "50%"
                            , style "height" "8px"
                            , style "width" "8px"
                            , style "background-color" "black"
                            , style "margin" "auto"
                            , style "margin-top" "2px"
                            , style "position" "absoulte"
                            , style "top" "12px"
                            ]
                            []
                        ]
                    )
                )
            ]
        ]


cellSize : Int
cellSize =
    100


gridDimensions : Int
gridDimensions =
    5


intToPx : Int -> String
intToPx val =
    String.fromInt val ++ "px"


translatePixels : Int -> String
translatePixels val =
    intToPx (val * cellSize)


cardinalButton : Model -> Orientation -> Html Msg
cardinalButton { keyMode } orientation =
    Html.button
        [ onMouseDown (GotInput orientation)
        , onMouseUp Interrupt
        ]
        [ Html.text
            (case ( keyMode, orientation ) of
                ( Advance, Left ) ->
                    "←"

                ( Rotate, Left ) ->
                    "⟲"

                ( Advance, Right ) ->
                    "→"

                ( Rotate, Right ) ->
                    "⟳"

                ( _, Up ) ->
                    "↑"

                ( _, Down ) ->
                    "↓"
            )
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions { movement } =
    Sub.batch
        [ onKeyUp
            (Decode.map
                (\string ->
                    case string of
                        " " ->
                            ToggleKeyMode

                        _ ->
                            Interrupt
                )
                (Decode.field "key" Decode.string)
            )
        , case movement of
            Idle ->
                onKeyDown keyDecoder

            _ ->
                onAnimationFrame (GotFrame << Time.posixToMillis)
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map
        (\string ->
            case string of
                "ArrowLeft" ->
                    GotInput Left

                "ArrowRight" ->
                    GotInput Right

                "ArrowUp" ->
                    GotInput Up

                "ArrowDown" ->
                    GotInput Down

                _ ->
                    Interrupt
        )
        (Decode.field "key" Decode.string)
