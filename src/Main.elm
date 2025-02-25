module Main exposing (main)

{-| You do not need to touch this file. Go to `src/App.elm` to paste in examples and fiddle around
-}

import Angle
import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import FastDict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Length exposing (Length)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task
import Time
import Vector2d exposing (Vector2d)


main : Platform.Program () State Event
main =
    Browser.element
        { init =
            \() ->
                ( initialState
                , Browser.Dom.getViewport
                    |> Task.perform
                        (\viewport ->
                            WindowSized
                                { width = viewport.viewport.width
                                , height = viewport.viewport.height
                                }
                        )
                )
        , view = view
        , update = \event state -> ( updateStateBasedOnEvent event state, Cmd.none )
        , subscriptions = subscribe
        }


type alias State =
    { windowWidth : Float
    , windowHeight : Float
    , lastSimulationTime : Maybe Time.Posix
    , body : Body
    , draggedBodyPointId : Maybe Int
    }


type alias Body =
    { bones :
        List
            { startPointId : Int
            , endPointId : Int
            , length : Length
            }
    , points :
        FastDict.Dict
            Int
            { position : Point2d Length.Meters Never
            , movement : BodyPointMovement
            }
    }


type BodyPointMovement
    = BodyPointFixed
    | BodyPointFree
        { velocity :
            Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Never
        }


initialState : State
initialState =
    { windowHeight = 1000
    , windowWidth = 2000
    , lastSimulationTime = Nothing
    , draggedBodyPointId = Nothing
    , body =
        { bones =
            [ { startPointId = 0
              , endPointId = 1
              , length = Length.meters 0.2
              }
            , { startPointId = 1
              , endPointId = 2
              , length = Length.meters 0.2
              }
            , { startPointId = 0
              , endPointId = 2
              , length = Length.meters 0.2
              }
            , { startPointId = 2
              , endPointId = 3
              , length = Length.meters 0.5
              }
            , { startPointId = 0
              , endPointId = 4
              , length = Length.meters 0.5
              }
            , { startPointId = 0
              , endPointId = 5
              , length = Length.meters 0.1
              }
            , { startPointId = 5
              , endPointId = 6
              , length = Length.meters 0.1
              }
            ]
        , points =
            FastDict.empty
                |> FastDict.insert 0
                    { position = Point2d.fromMeters { x = 0.2, y = 0.2 }
                    , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0.5 0
                                    |> Vector2d.per Duration.second
                            }
                    }
                |> FastDict.insert 1
                    { position = Point2d.fromMeters { x = 0.4, y = 0.4 }
                    , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0.5 0
                                    |> Vector2d.per Duration.second
                            }
                    }
                |> FastDict.insert 2
                    { position = Point2d.fromMeters { x = 0.6, y = 0.4 }
                    , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0.2 0
                                    |> Vector2d.per Duration.second
                            }
                    }
                |> FastDict.insert 3
                    { position = Point2d.fromMeters { x = 0.65, y = 0.6 }
                    , movement =
                        BodyPointFixed
                    }
                |> FastDict.insert 4
                    { position = Point2d.fromMeters { x = 0.15, y = 0.6 }
                    , movement =
                        BodyPointFixed
                    }
                |> FastDict.insert 5
                    { position = Point2d.fromMeters { x = 0.15, y = 0.1 }
                    , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0 0
                                    |> Vector2d.per Duration.second
                            }
                    }
                |> FastDict.insert 6
                    { position = Point2d.fromMeters { x = 0.075, y = 0.15 }
                    , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0 0
                                    |> Vector2d.per Duration.second
                            }
                    }
        }
    }


type Event
    = WindowSized { width : Float, height : Float }
    | MouseReleased
    | SimulationTickPassed Time.Posix
    | MouseMoved
        { x : Float
        , y : Float
        , draggedBodyPointId : Int
        }
    | MousePressed { draggedBodyPointId : Int }


updateStateBasedOnEvent : Event -> State -> State
updateStateBasedOnEvent event state =
    case event of
        WindowSized newWindowSize ->
            { state
                | windowWidth = newWindowSize.width
                , windowHeight = newWindowSize.height
            }

        MouseReleased ->
            { state | draggedBodyPointId = Nothing }

        MouseMoved moved ->
            { state
                | body =
                    state.body
                        |> (\body ->
                                { body
                                    | points =
                                        body.points
                                            |> FastDict.update
                                                moved.draggedBodyPointId
                                                (Maybe.map
                                                    (\draggedPoint ->
                                                        { draggedPoint
                                                            | position =
                                                                Point2d.fromMeters
                                                                    { x = moved.x / 1000
                                                                    , y = moved.y / 1000
                                                                    }
                                                        }
                                                    )
                                                )
                                }
                           )
            }

        SimulationTickPassed currentTime ->
            let
                sincePreviousSimulation : Duration
                sincePreviousSimulation =
                    case state.lastSimulationTime of
                        Nothing ->
                            Duration.seconds 0

                        Just previousSimulationTime ->
                            Duration.from previousSimulationTime currentTime
            in
            { state
                | lastSimulationTime = Just currentTime
                , body =
                    state.body
                        |> bodyVelocityAlter
                            (\velocity ->
                                velocity
                                    |> Vector2d.plus
                                        (gravity
                                            |> Vector2d.for sincePreviousSimulation
                                        )
                            )
                        |> bodyApplyForcesFromBones
                        |> bodyVelocityAlter
                            (\velocity ->
                                velocity
                                    |> Vector2d.multiplyBy
                                        (1
                                            - (velocityLossScalePerSecond
                                                * (sincePreviousSimulation |> Duration.inSeconds)
                                              )
                                        )
                            )
                        |> bodyUpdatePointPositions sincePreviousSimulation
            }

        MousePressed pressed ->
            { state
                | draggedBodyPointId = Just pressed.draggedBodyPointId
            }


subscribe : State -> Sub Event
subscribe state =
    Sub.batch
        [ Browser.Events.onResize
            (\width height ->
                WindowSized
                    { width = width |> Basics.toFloat
                    , height = height |> Basics.toFloat
                    }
            )
        , Browser.Events.onMouseUp
            (Json.Decode.map (\() -> MouseReleased)
                (Json.Decode.succeed ())
            )
        , Time.every (1000 / 60)
            SimulationTickPassed
        ]


bodyApplyForcesFromBones : Body -> Body
bodyApplyForcesFromBones body =
    { body
        | points =
            body.bones
                |> List.foldl
                    (\bone bodyPointsSoFar ->
                        case
                            ( bodyPointsSoFar |> FastDict.get bone.startPointId
                            , bodyPointsSoFar |> FastDict.get bone.endPointId
                            )
                        of
                            ( Just startPoint, Just endPoint ) ->
                                let
                                    startToEndVector : Vector2d Length.Meters Never
                                    startToEndVector =
                                        Vector2d.from startPoint.position endPoint.position

                                    startToEndDirection : Direction2d Never
                                    startToEndDirection =
                                        startToEndVector
                                            |> Vector2d.direction
                                            |> Maybe.withDefault Direction2d.positiveY

                                    actualDistance : Length
                                    actualDistance =
                                        startToEndVector
                                            |> Vector2d.length

                                    lengthDifferenceToBalance : Length
                                    lengthDifferenceToBalance =
                                        actualDistance
                                            |> Quantity.minus bone.length
                                in
                                case ( startPoint.movement, endPoint.movement ) of
                                    ( BodyPointFixed, BodyPointFixed ) ->
                                        bodyPointsSoFar

                                    ( BodyPointFree startMovementFree, BodyPointFixed ) ->
                                        bodyPointsSoFar
                                            |> FastDict.insert bone.startPointId
                                                { startPoint
                                                    | movement =
                                                        BodyPointFree
                                                            { velocity =
                                                                startMovementFree.velocity
                                                                    |> Vector2d.plus
                                                                        (Vector2d.withLength
                                                                            lengthDifferenceToBalance
                                                                            startToEndDirection
                                                                            |> Vector2d.per boneLooseness
                                                                        )
                                                            }
                                                }

                                    ( BodyPointFixed, BodyPointFree endMovementFree ) ->
                                        bodyPointsSoFar
                                            |> FastDict.insert bone.endPointId
                                                { endPoint
                                                    | movement =
                                                        BodyPointFree
                                                            { velocity =
                                                                endMovementFree.velocity
                                                                    |> Vector2d.plus
                                                                        (Vector2d.withLength
                                                                            lengthDifferenceToBalance
                                                                            (startToEndDirection
                                                                                |> Direction2d.reverse
                                                                            )
                                                                            |> Vector2d.per boneLooseness
                                                                        )
                                                            }
                                                }

                                    ( BodyPointFree startMovementFree, BodyPointFree endMovementFree ) ->
                                        bodyPointsSoFar
                                            |> FastDict.insert bone.startPointId
                                                { startPoint
                                                    | movement =
                                                        BodyPointFree
                                                            { velocity =
                                                                startMovementFree.velocity
                                                                    |> Vector2d.plus
                                                                        (Vector2d.withLength
                                                                            lengthDifferenceToBalance
                                                                            startToEndDirection
                                                                            |> Vector2d.per boneLooseness
                                                                        )
                                                            }
                                                }
                                            |> FastDict.insert bone.endPointId
                                                { endPoint
                                                    | movement =
                                                        BodyPointFree
                                                            { velocity =
                                                                endMovementFree.velocity
                                                                    |> Vector2d.plus
                                                                        (Vector2d.withLength
                                                                            lengthDifferenceToBalance
                                                                            (startToEndDirection
                                                                                |> Direction2d.reverse
                                                                            )
                                                                            |> Vector2d.per boneLooseness
                                                                        )
                                                            }
                                                }

                            _ ->
                                bodyPointsSoFar
                    )
                    body.points
    }


velocityLossScalePerSecond : Float
velocityLossScalePerSecond =
    2.1


gravity : Vector2d (Quantity.Rate (Quantity.Rate Length.Meters Duration.Seconds) Duration.Seconds) Never
gravity =
    Vector2d.meters 0 0.64
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


boneLooseness : Duration
boneLooseness =
    Duration.seconds 0.13


bodyUpdatePointPositions : Duration -> Body -> Body
bodyUpdatePointPositions sincePreviousSimulation body =
    { body
        | points =
            body.points
                |> FastDict.map
                    (\_ point ->
                        case point.movement of
                            BodyPointFixed ->
                                point

                            BodyPointFree pointMovementFree ->
                                { point
                                    | position =
                                        point.position
                                            |> Point2d.translateBy
                                                (pointMovementFree.velocity
                                                    |> Vector2d.for sincePreviousSimulation
                                                )
                                }
                    )
    }


bodyVelocityAlter :
    (Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Never
     -> Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Never
    )
    -> Body
    -> Body
bodyVelocityAlter velocityChange body =
    { points =
        body.points
            |> FastDict.map
                (\_ point ->
                    case point.movement of
                        BodyPointFixed ->
                            point

                        BodyPointFree pointMovementFree ->
                            { point
                                | movement =
                                    BodyPointFree
                                        { velocity =
                                            pointMovementFree.velocity |> velocityChange
                                        }
                            }
                )
    , bones = body.bones
    }


view : State -> Html Event
view state =
    Html.div
        [ Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "background" "black"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "margin" "0px"
        ]
        [ Svg.svg
            ([ Svg.Attributes.viewBox
                ("0 0 "
                    ++ (state.windowWidth |> String.fromFloat)
                    ++ " "
                    ++ (state.windowHeight |> String.fromFloat)
                )
             , Html.Attributes.style "background" "red"
             , Html.Attributes.style "height" "100vh"
             , Html.Attributes.style "width" "100vw"
             , Html.Attributes.style "margin" "0px"
             ]
                ++ (case state.draggedBodyPointId of
                        Nothing ->
                            []

                        Just draggedBodyPointId ->
                            [ Html.Events.on "mousemove"
                                (Json.Decode.map2
                                    (\x y ->
                                        MouseMoved
                                            { x = x
                                            , y = y
                                            , draggedBodyPointId = draggedBodyPointId
                                            }
                                    )
                                    (Json.Decode.field "clientX" Json.Decode.float)
                                    (Json.Decode.field "clientY" Json.Decode.float)
                                )
                            ]
                   )
            )
            [ Svg.g
                [ svgScaled 1000
                ]
                [ state.body.bones
                    |> List.map
                        (\bone ->
                            case
                                ( state.body.points |> FastDict.get bone.startPointId
                                , state.body.points |> FastDict.get bone.endPointId
                                )
                            of
                                ( Just startPoint, Just endPoint ) ->
                                    svgBone
                                        { start = startPoint.position |> Point2d.toMeters
                                        , end = endPoint.position |> Point2d.toMeters
                                        }
                                        []

                                _ ->
                                    Html.text "invalid point index"
                        )
                    |> Svg.g []
                , state.body.points
                    |> FastDict.toList
                    |> List.map
                        (\( pointIndex, point ) ->
                            svgCircle
                                { position = point.position |> Point2d.toMeters
                                , radius = 0.02
                                }
                                ([ Svg.Events.on "mousedown"
                                    (Json.Decode.map
                                        (\() -> MousePressed { draggedBodyPointId = pointIndex })
                                        (Json.Decode.succeed ())
                                    )
                                 ]
                                    ++ (if state.draggedBodyPointId == Just pointIndex then
                                            [ svgFillUniform (Color.rgb 0 0.33 0.23)
                                            , svgStrokeWidth 0.005
                                            , svgStrokeUniform (Color.rgb 0.4 0.55 0.55)
                                            ]

                                        else
                                            [ svgFillUniform (Color.rgb 0 0 0) ]
                                       )
                                )
                        )
                    |> Svg.g []
                ]
            ]
        , Html.text (Debug.toString { width = state.windowWidth })
        ]


svgBone :
    { start : { x : Float, y : Float }
    , end : { x : Float, y : Float }
    }
    -> List (Svg.Attribute event)
    -> Svg event
svgBone line additionalModifiers =
    Svg.g additionalModifiers
        [ svgLine line
            [ svgStrokeWidth 0.01
            , svgStrokeUniform (Color.rgb 0.2 0.2 0.2)
            , Svg.Attributes.strokeLinecap "round"
            ]
        , svgCircle
            { radius = 0.002
            , position = line.start
            }
            [ svgFillUniform (Color.rgb 0.8 0.8 0.8)
            ]
        , svgCircle
            { radius = 0.002
            , position = line.end
            }
            [ svgFillUniform (Color.rgb 0.8 0.8 0.8)
            ]
        ]


svgLine :
    { start : { x : Float, y : Float }
    , end : { x : Float, y : Float }
    }
    -> List (Svg.Attribute event)
    -> Svg event
svgLine lineGeometry additionalModifiers =
    Svg.line
        ([ Svg.Attributes.x1 ((lineGeometry.start.x |> String.fromFloat) ++ "px")
         , Svg.Attributes.y1 ((lineGeometry.start.y |> String.fromFloat) ++ "px")
         , Svg.Attributes.x2 ((lineGeometry.end.x |> String.fromFloat) ++ "px")
         , Svg.Attributes.y2 ((lineGeometry.end.y |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


svgCircle :
    { position : { x : Float, y : Float }
    , radius : Float
    }
    -> List (Svg.Attribute event)
    -> Svg event
svgCircle geometry additionalModifiers =
    Svg.circle
        ([ Svg.Attributes.cx ((geometry.position.x |> String.fromFloat) ++ "px")
         , Svg.Attributes.cy ((geometry.position.y |> String.fromFloat) ++ "px")
         , Svg.Attributes.r ((geometry.radius |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


svgEllipse :
    { position : { x : Float, y : Float }
    , radiusX : Float
    , radiusY : Float
    }
    -> List (Svg.Attribute event)
    -> Svg event
svgEllipse geometry additionalModifiers =
    Svg.ellipse
        ([ Svg.Attributes.cx ((geometry.position.x |> String.fromFloat) ++ "px")
         , Svg.Attributes.cy ((geometry.position.y |> String.fromFloat) ++ "px")
         , Svg.Attributes.rx ((geometry.radiusX |> String.fromFloat) ++ "px")
         , Svg.Attributes.ry ((geometry.radiusY |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


svgPolygon :
    List { x : Float, y : Float }
    -> List (Svg.Attribute event)
    -> Svg event
svgPolygon points_ additionalModifiers =
    Svg.polyline
        (svgPoints points_
            :: additionalModifiers
        )
        []


svgPolyline :
    List { x : Float, y : Float }
    -> List (Svg.Attribute event)
    -> Svg event
svgPolyline points_ additionalModifiers =
    Svg.polyline
        (svgPoints points_
            :: additionalModifiers
        )
        []


svgOffsetBy : { x : Float, y : Float } -> Svg msg -> Svg msg
svgOffsetBy offset svg =
    Svg.g
        [ Svg.Attributes.transform
            ("translate("
                ++ (offset.x |> String.fromFloat)
                ++ ", "
                ++ (offset.y |> String.fromFloat)
                ++ ")"
            )
        ]
        [ svg ]


svgOpacity : Float -> Svg.Attribute event_
svgOpacity percentage =
    Svg.Attributes.opacity (percentage |> String.fromFloat)


svgScaled : Float -> Svg.Attribute event_
svgScaled scale =
    Svg.Attributes.transform
        ([ "scale("
         , scale |> String.fromFloat
         , ")"
         ]
            |> String.concat
        )


svgRotated :
    { angle : Angle.Angle
    , center : { x : Float, y : Float }
    }
    -> Svg.Attribute event_
svgRotated geometry =
    Svg.Attributes.transform
        ([ "rotate("
         , geometry.angle |> Angle.inDegrees |> String.fromFloat
         , ", "
         , geometry.center.x |> String.fromFloat
         , ", "
         , geometry.center.y |> String.fromFloat
         , ")"
         ]
            |> String.concat
        )


svgFillUniform : Color -> Svg.Attribute event_
svgFillUniform color =
    Svg.Attributes.fill (color |> Color.toCssString)


svgStrokeWidth : Float -> Svg.Attribute event_
svgStrokeWidth pixels =
    Svg.Attributes.strokeWidth ((pixels |> String.fromFloat) ++ "px")


svgStrokeUniform : Color -> Svg.Attribute event_
svgStrokeUniform color =
    Svg.Attributes.stroke (color |> Color.toCssString)


svgPoints : List { x : Float, y : Float } -> Svg.Attribute event_
svgPoints xys =
    Svg.Attributes.points
        ((case xys of
            [ onlyElement ] ->
                [ onlyElement, onlyElement ]

            notOnlyOne ->
                notOnlyOne
         )
            |> List.map
                (\point ->
                    (point.x |> String.fromFloat)
                        ++ ","
                        ++ (point.y |> String.fromFloat)
                )
            |> String.join " "
        )
