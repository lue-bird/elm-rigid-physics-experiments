module Main exposing (main)

{-| You do not need to touch this file. Go to `src/App.elm` to paste in examples and fiddle around
-}

import Angle
import Array exposing (Array)
import Browser
import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes
import Json.Encode
import Length exposing (Length)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Svg.Attributes
import Time
import Vector2d exposing (Vector2d)


main : Platform.Program () State State
main =
    Browser.element
        { init = \() -> ( initialState, Cmd.none )
        , view = view
        , update = \newState _ -> ( newState, Cmd.none )
        , subscriptions = subscribe
        }


type alias State =
    { lastSimulationTime : Maybe Time.Posix
    , body : Body
    }


type alias Body =
    { bones :
        List
            { startIndex : Int
            , endIndex : Int
            , length : Length
            }
    , points :
        Array
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
    { lastSimulationTime = Nothing
    , body =
        { bones =
            [ { startIndex = 0
              , endIndex = 1
              , length = Length.meters 0.2
              }
            , { startIndex = 1
              , endIndex = 2
              , length = Length.meters 0.2
              }
            , { startIndex = 0
              , endIndex = 2
              , length = Length.meters 0.2
              }
            , { startIndex = 2
              , endIndex = 3
              , length = Length.meters 0.5
              }
            , { startIndex = 0
              , endIndex = 4
              , length = Length.meters 0.5
              }
            , { startIndex = 0
              , endIndex = 5
              , length = Length.meters 0.1
              }
            , { startIndex = 5
              , endIndex = 6
              , length = Length.meters 0.1
              }
            ]
        , points =
            Array.fromList
                [ { position = Point2d.fromMeters { x = 0.2, y = 0.2 }
                  , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0.5 0
                                    |> Vector2d.per Duration.second
                            }
                  }
                , { position = Point2d.fromMeters { x = 0.4, y = 0.4 }
                  , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0.5 0
                                    |> Vector2d.per Duration.second
                            }
                  }
                , { position = Point2d.fromMeters { x = 0.6, y = 0.4 }
                  , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0.2 0
                                    |> Vector2d.per Duration.second
                            }
                  }
                , { position = Point2d.fromMeters { x = 0.65, y = 0.6 }
                  , movement =
                        BodyPointFixed
                  }
                , { position = Point2d.fromMeters { x = 0.15, y = 0.6 }
                  , movement =
                        BodyPointFixed
                  }
                , { position = Point2d.fromMeters { x = 0.15, y = 0.1 }
                  , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0 0
                                    |> Vector2d.per Duration.second
                            }
                  }
                , { position = Point2d.fromMeters { x = 0.075, y = 0.15 }
                  , movement =
                        BodyPointFree
                            { velocity =
                                Vector2d.meters 0 0
                                    |> Vector2d.per Duration.second
                            }
                  }
                ]
        }
    }


subscribe : State -> Sub State
subscribe state =
    Time.every (1000 / 60)
        (\currentTime ->
            let
                sincePreviousSimulation : Duration
                sincePreviousSimulation =
                    case state.lastSimulationTime of
                        Nothing ->
                            Duration.seconds 0

                        Just previousSimulationTime ->
                            Duration.from previousSimulationTime currentTime

                bodyWithGravity : Body
                bodyWithGravity =
                    state.body
                        |> bodyVelocityAlter
                            (\velocity ->
                                velocity
                                    |> Vector2d.plus
                                        (gravity
                                            |> Vector2d.for sincePreviousSimulation
                                        )
                            )

                bodyWithForcesFromBonesApplies : Body
                bodyWithForcesFromBonesApplies =
                    { bodyWithGravity
                        | points =
                            bodyWithGravity.bones
                                |> List.foldl
                                    (\bone bodyPointsSoFar ->
                                        case
                                            ( bodyPointsSoFar |> Array.get bone.startIndex
                                            , bodyPointsSoFar |> Array.get bone.endIndex
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
                                                            |> Array.set bone.startIndex
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
                                                            |> Array.set bone.endIndex
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
                                                            |> Array.set bone.startIndex
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
                                                            |> Array.set bone.endIndex
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
                                    bodyWithGravity.points
                    }
            in
            { state
                | lastSimulationTime = Just currentTime
                , body =
                    bodyWithForcesFromBonesApplies
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
        )


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
                |> Array.map
                    (\point ->
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
            |> Array.map
                (\point ->
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


view : State -> Html State
view state =
    Html.div
        [ Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "background" "black"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "margin" "0px"
        ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "0 0 1.6 0.9"
            , Html.Attributes.style "background" "red"
            , Html.Attributes.style "height" "100vh"
            , Html.Attributes.style "width" "100vw"
            , Html.Attributes.style "margin" "0px"
            ]
            [ state.body.bones
                |> List.map
                    (\bone ->
                        case
                            ( state.body.points |> Array.get bone.startIndex
                            , state.body.points |> Array.get bone.endIndex
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
            ]
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
