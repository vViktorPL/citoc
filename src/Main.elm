module Main exposing (..)

import Html exposing (Html)
import Browser
import Scene3d
import Color exposing (..)
import Direction3d
import Angle
import Length
import Pixels
import Camera3d
import Viewpoint3d
import Point3d
import Scene3d.Material
import Block3d
import Browser.Events
import Json.Decode as Decode

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationTick
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        ]


type WorldCoordinates = WorldCoordinates

type MapCoordinates = MapCoordinates

-- MODEL

type alias Model =
    { position: Point3d.Point3d Length.Meters WorldCoordinates
    , angle: Angle.Angle
    , control: PlayerControlState
    }

type alias PlayerControlState =
    { turn: TurnControl
    , move: MoveControl
    }

type TurnControl = NoTurn | TurnLeft | TurnRight

type MoveControl = Stand | Forward | Backward

initialModel : Model
initialModel =
    { position = Point3d.meters 5 10 0.5
    , angle = Angle.degrees 180
    , control = { turn = NoTurn, move = Stand }
    }


pointOnMap : Float -> Float -> Float -> Point3d.Point3d Length.Meters WorldCoordinates
pointOnMap x y z =
    Point3d.meters -x y z


-- UPDATE

type Msg
    = AnimationTick Float
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationTick delta ->
            ( model |> updatePlayerAngle delta |> updatePlayerPosition delta, Cmd.none )

        KeyDown key ->
            case key of
                "ArrowRight" -> (updatePlayerTurning model TurnRight, Cmd.none)
                "ArrowLeft" -> (updatePlayerTurning model TurnLeft, Cmd.none)
                "ArrowUp" -> (updatePlayerMovement model Forward, Cmd.none)
                "ArrowDown" -> (updatePlayerMovement model Backward, Cmd.none)
                _ -> (model, Cmd.none)

        KeyUp key ->
            case key of
                "ArrowRight" -> (updatePlayerTurning model NoTurn, Cmd.none)
                "ArrowLeft" -> (updatePlayerTurning model NoTurn, Cmd.none)
                "ArrowUp" -> (updatePlayerMovement model Stand, Cmd.none)
                "ArrowDown" -> (updatePlayerMovement model Stand, Cmd.none)
                _ -> (model, Cmd.none)

updatePlayerTurning : Model -> TurnControl -> Model
updatePlayerTurning model turning =
    let
        control = model.control
    in
        { model | control = { control | turn = turning } }

updatePlayerMovement : Model -> MoveControl -> Model
updatePlayerMovement model movement =
    let
        control = model.control
    in
        { model | control = { control | move = movement } }


playerRotationSpeed = 0.05
playerMovementSpeed = 0.02

updatePlayerAngle : Float -> Model -> Model
updatePlayerAngle delta model =
    case model.control.turn of
        TurnRight ->
            { model |
                angle = Angle.degrees ((Angle.inDegrees model.angle) + delta * playerRotationSpeed)
            }
        TurnLeft ->
            { model |
                angle = Angle.degrees ((Angle.inDegrees model.angle) - delta * playerRotationSpeed)
            }

        NoTurn -> model



updatePlayerPosition : Float -> Model -> Model
updatePlayerPosition delta model =
    let
           direction = Direction3d.yx model.angle
           velocity = delta * playerMovementSpeed
    in
    case model.control.move of
        Forward -> { model | position = Point3d.translateIn direction (Length.meters velocity) model.position }
        Backward -> { model | position = Point3d.translateIn direction (Length.meters -velocity) model.position }
        Stand -> model

keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string

playerViewpoint : Model -> (Viewpoint3d.Viewpoint3d Length.Meters WorldCoordinates)
playerViewpoint model =
    let
       direction = Direction3d.yx model.angle
    in
    Viewpoint3d.lookAt
        { eyePoint = model.position
        , focalPoint = Point3d.translateIn direction Length.meter model.position
        , upDirection = Direction3d.positiveZ
        }





-- VIEW

view : Model -> Html Msg
view model =
    Scene3d.sunny
        { entities = [ Scene3d.block (Scene3d.Material.matte Color.darkRed)
                (Block3d.with
                { x1 = Length.meters 0
                , x2 = Length.meters 10
                , y1 = Length.meters 0
                , y2 = Length.meters 1
                , z1 = Length.meters 0
                , z2 = Length.meters 2
                })  ]
        , camera =
              Camera3d.perspective
                  { viewpoint = playerViewpoint model
                  , verticalFieldOfView = Angle.degrees 45
                  }
        , upDirection = Direction3d.z
        , sunlightDirection = Direction3d.xz (Angle.degrees -120)
        , background = Scene3d.backgroundColor Color.white
        , clipDepth = Length.centimeters 1
        , shadows = True
        , dimensions = ( Pixels.int 800, Pixels.int 600 )
        }
