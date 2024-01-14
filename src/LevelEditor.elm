module LevelEditor exposing (Model, Msg, init, subscription, update, view)

import Angle
import Assets
import Axis3d
import Block3d
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Coordinates exposing (EditorScreenCoordinates, EditorWorldCoordinates, SectorCoordinates)
import Dict exposing (Dict)
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as D
import Length exposing (Length, Meters)
import LevelTile
import Orientation exposing (Orientation)
import Pixels exposing (pixels)
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Rectangle2d
import Scene3d
import Scene3d.Material
import Task
import Triangle3d
import Trigger
import Viewpoint3d


type Model
    = LevelEditor EditorState


type EditorTool
    = LevelSettings
    | Camera
    | TilePainter
    | TriggersManager


type alias EditorState =
    { assets : Assets.Model
    , tiles : Dict SectorCoordinates ( LevelTile.Model, List Trigger.Trigger )
    , selectedTool : EditorTool
    , currentTile : LevelTile.Model
    , triggers : List Trigger.Trigger
    , globalTriggers : List Trigger.Trigger
    , zoom : Length
    , focalPoint : Point3d Meters EditorWorldCoordinates
    , playerStartPosition : SectorCoordinates
    , playerStartingOrientation : Orientation
    , windowSize : ( Int, Int )
    , selectedSector : SectorCoordinates
    , isMouseDown : Bool
    , mouseDownAtPoint : Maybe (Point3d Meters EditorWorldCoordinates)
    , selectingPlayerPosition : Bool
    }


type alias SectorData =
    { tile : LevelTile.Model
    , triggers : List Trigger.Trigger
    }


type TileConfiguratorState
    = EmptyTile
    | WallTile
    | FloorTile { withCeiling : Bool }
    | SignTile { orientation : Orientation, content : String }
    | SandTile { withCeiling : Bool }


type Msg
    = ScrollView ( Float, Float )
    | AssetsMsg Assets.Msg
    | WindowResize Int Int
    | MouseMove Int Int
    | MouseDown Int Int
    | MouseUp
    | TileConfigMsg LevelTile.ConfigMsg
    | SelectTool EditorTool
    | ZoomChange Float
    | TogglePlayerPositionSelection
    | ChangePlayerStartingOrientation Orientation
    | UpdateTriggerCondition (Maybe SectorCoordinates) Int TriggerConditionMsg


init : ( Int, Int ) -> ( Model, Cmd Msg )
init windowSize =
    let
        ( assets, assetsCmd ) =
            Assets.init
                |> Assets.requestDependencies (LevelTile.dependencies LevelTile.wall ++ LevelTile.dependencies LevelTile.floor)
    in
    ( LevelEditor
        { assets = assets
        , windowSize = windowSize
        , selectedTool = LevelSettings
        , tiles = Dict.fromList [ ( ( 0, 0 ), ( LevelTile.wall, [] ) ), ( ( 0, 1 ), ( LevelTile.floor, [] ) ) ]
        , currentTile = LevelTile.wall
        , globalTriggers = []
        , triggers = []
        , zoom = Length.meters 10
        , focalPoint = Point3d.meters 2 7 0
        , selectedSector = ( 0, 0 )
        , isMouseDown = False
        , mouseDownAtPoint = Nothing
        , playerStartPosition = ( 0, 0 )
        , playerStartingOrientation = Orientation.South
        , selectingPlayerPosition = False
        }
    , Cmd.batch
        [ assetsCmd |> Cmd.map AssetsMsg
        , Task.perform
            (\viewportDetails -> WindowResize (floor viewportDetails.viewport.width) (floor viewportDetails.viewport.height))
            Browser.Dom.getViewport
        ]
    )


placeTile : EditorState -> EditorState
placeTile state =
    { state
        | tiles =
            Dict.update
                state.selectedSector
                (\maybeExistingTile ->
                    case maybeExistingTile of
                        Just ( existingTile, existingTriggers ) ->
                            Just ( state.currentTile, existingTriggers )

                        Nothing ->
                            Just ( state.currentTile, [] )
                )
                state.tiles
    }


mouseCoordinatesToWorldCoordinates : EditorState -> Int -> Int -> Maybe (Point3d Meters EditorWorldCoordinates)
mouseCoordinatesToWorldCoordinates state x y =
    let
        ( width, height ) =
            state.windowSize

        ray =
            Camera3d.ray
                (camera state)
                (Rectangle2d.with
                    { x1 = pixels 0
                    , y1 = pixels (toFloat height)
                    , x2 = pixels (toFloat width)
                    , y2 = pixels 0
                    }
                )
                (Point2d.pixels (toFloat x) (toFloat y))
    in
    Axis3d.intersectionWithPlane Plane3d.xy ray


pointToSector : Point3d Meters Coordinates.EditorWorldCoordinates -> SectorCoordinates
pointToSector p =
    let
        sectorX =
            p
                |> Point3d.xCoordinate
                |> Length.inMeters
                |> ceiling
                |> (\n -> -n)

        sectorY =
            p
                |> Point3d.yCoordinate
                |> Length.inMeters
                |> floor
    in
    ( sectorX, sectorY )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (LevelEditor state) =
    case msg of
        SelectTool newTool ->
            ( LevelEditor { state | selectedTool = newTool }, Cmd.none )

        AssetsMsg assetsMsg ->
            let
                ( updatedAssets, assetsCmd ) =
                    Assets.update assetsMsg state.assets
            in
            ( LevelEditor { state | assets = updatedAssets }, Cmd.map AssetsMsg assetsCmd )

        WindowResize width height ->
            ( LevelEditor { state | windowSize = ( width, height ) }, Cmd.none )

        MouseDown x y ->
            case state.selectedTool of
                TilePainter ->
                    let
                        requiredDeps =
                            LevelTile.dependencies state.currentTile

                        ( updatedAssets, assetsCmd ) =
                            Assets.requestDependencies requiredDeps state.assets
                    in
                    ( LevelEditor (placeTile { state | isMouseDown = True, assets = updatedAssets }), assetsCmd |> Cmd.map AssetsMsg )

                LevelSettings ->
                    if state.selectingPlayerPosition then
                        ( LevelEditor { state | playerStartPosition = state.selectedSector, selectingPlayerPosition = False }, Cmd.none )

                    else
                        ( LevelEditor state, Cmd.none )

                _ ->
                    ( LevelEditor
                        { state
                            | isMouseDown = True
                            , mouseDownAtPoint = mouseCoordinatesToWorldCoordinates state x y
                        }
                    , Cmd.none
                    )

        MouseUp ->
            ( LevelEditor { state | isMouseDown = False, mouseDownAtPoint = Nothing }, Cmd.none )

        MouseMove x y ->
            case ( mouseCoordinatesToWorldCoordinates state x y, state.selectedTool ) of
                ( Just intersectionPoint, TilePainter ) ->
                    ( { state | selectedSector = pointToSector intersectionPoint }
                        |> (if state.isMouseDown then
                                placeTile

                            else
                                identity
                           )
                        |> LevelEditor
                    , Cmd.none
                    )

                ( Just intersectionPoint, LevelSettings ) ->
                    if state.selectingPlayerPosition then
                        ( LevelEditor { state | selectedSector = pointToSector intersectionPoint }, Cmd.none )

                    else
                        ( LevelEditor state, Cmd.none )

                ( Just intersectionPoint, Camera ) ->
                    case state.mouseDownAtPoint of
                        Just mouseDownAtPoint ->
                            let
                                currentX =
                                    state.focalPoint
                                        |> Point3d.xCoordinate
                                        |> Length.inMeters

                                currentY =
                                    state.focalPoint
                                        |> Point3d.yCoordinate
                                        |> Length.inMeters

                                mouseDownX =
                                    mouseDownAtPoint
                                        |> Point3d.xCoordinate
                                        |> Length.inMeters

                                mouseDownY =
                                    mouseDownAtPoint
                                        |> Point3d.yCoordinate
                                        |> Length.inMeters

                                intersectionX =
                                    intersectionPoint
                                        |> Point3d.xCoordinate
                                        |> Length.inMeters

                                intersectionY =
                                    intersectionPoint
                                        |> Point3d.yCoordinate
                                        |> Length.inMeters
                            in
                            ( LevelEditor
                                { state
                                    | focalPoint =
                                        Point3d.meters
                                            (currentX - (intersectionX - mouseDownX))
                                            (currentY - (intersectionY - mouseDownY))
                                            0
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( LevelEditor state, Cmd.none )

                _ ->
                    ( LevelEditor state, Cmd.none )

        TileConfigMsg tileConfigMsg ->
            ( LevelEditor { state | currentTile = LevelTile.updateConfig tileConfigMsg state.currentTile }, Cmd.none )

        ZoomChange multiplier ->
            ( LevelEditor
                { state
                    | zoom =
                        state.zoom
                            |> Length.inMeters
                            |> (*) multiplier
                            |> Length.meters
                }
            , Cmd.none
            )

        TogglePlayerPositionSelection ->
            ( LevelEditor
                { state
                    | selectingPlayerPosition = not state.selectingPlayerPosition
                }
            , Cmd.none
            )

        ChangePlayerStartingOrientation newOrientation ->
            ( LevelEditor
                { state | playerStartingOrientation = newOrientation }
            , Cmd.none
            )

        _ ->
            ( LevelEditor state, Cmd.none )


camera : EditorState -> Camera3d Meters EditorWorldCoordinates
camera { zoom, focalPoint } =
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.isometric
                { focalPoint = focalPoint
                , distance =
                    zoom
                        |> Length.inMeters
                        |> max 10
                        |> Length.meters
                }
        , viewportHeight = zoom
        }


tileSelectionMaterial =
    Scene3d.Material.color (Color.fromRgba { red = 0, green = 0.8, blue = 0.2, alpha = 0.5 })


tileSelectionEntity =
    Block3d.from
        (Point3d.meters -0.51 -0.51 0)
        (Point3d.meters 0.51 0.51 0.15)
        |> Scene3d.block tileSelectionMaterial


viewPlayer state =
    Scene3d.triangle
        (Scene3d.Material.color Color.red)
        (Triangle3d.from
            (Point3d.meters 0 -0.2 0.01)
            (Point3d.meters 0.15 0.2 0.01)
            (Point3d.meters -0.15 0.2 0.01)
        )
        |> Scene3d.rotateAround
            Axis3d.z
            (case state.playerStartingOrientation of
                Orientation.North ->
                    Angle.degrees 0

                Orientation.East ->
                    Angle.degrees -90

                Orientation.South ->
                    Angle.degrees 180

                Orientation.West ->
                    Angle.degrees 90
            )
        |> Scene3d.placeIn (Frame3d.atPoint (Coordinates.sectorToEditorWorldPosition state.playerStartPosition))


view : Model -> Html Msg
view (LevelEditor state) =
    let
        showTileSelection =
            state.selectedTool == TilePainter || state.selectedTool == LevelSettings && state.selectingPlayerPosition

        tileEntities =
            state.tiles
                |> Dict.toList
                |> List.map
                    (\( sector, ( tile, triggers ) ) ->
                        LevelTile.editorView state.assets tile
                            |> Scene3d.placeIn (Frame3d.atPoint (Coordinates.sectorToEditorWorldPosition sector))
                    )
    in
    Html.div
        [ Attr.id "editor"
        , Attr.classList
            [ ( "cameraMode", state.selectedTool == Camera )
            , ( "crosshair", showTileSelection )
            ]
        ]
        [ Scene3d.cloudy
            { entities =
                (if showTileSelection then
                    [ tileSelectionEntity
                        |> Scene3d.placeIn (Frame3d.atPoint (Coordinates.sectorToEditorWorldPosition state.selectedSector))
                    ]

                 else
                    []
                )
                    ++ [ viewPlayer state ]
                    ++ tileEntities
            , camera = camera state
            , upDirection = Direction3d.z
            , background = Scene3d.backgroundColor Color.white
            , clipDepth = Length.centimeters 1
            , dimensions = Tuple.mapBoth Pixels.int Pixels.int state.windowSize
            , visibility = Scene3d.clearView
            }
        , Html.div
            [ Attr.class "panel" ]
            [ Html.div []
                [ toolButton state LevelSettings "⚙️ _S_ettings"
                , toolButton state Camera "🎥 _C_amera"
                , toolButton state TilePainter "🖌️ Tile _p_ainter"
                , toolButton state TriggersManager "✨ _T_riggers"
                ]
            , case state.selectedTool of
                LevelSettings ->
                    viewLevelSettingsTool state

                TilePainter ->
                    viewTilePainterTool state

                Camera ->
                    viewCameraTool state

                _ ->
                    Html.div [] [ Html.text "TODO" ]
            ]
        ]


toolButton : EditorState -> EditorTool -> String -> Html Msg
toolButton state tool label =
    Html.button
        [ Html.Events.onClick (SelectTool tool), Attr.classList [ ( "active", state.selectedTool == tool ) ] ]
        (String.split "_" label
            |> List.indexedMap
                (\index text ->
                    if index == 1 then
                        Html.u [] [ Html.text text ]

                    else
                        Html.text text
                )
        )


sectorCoordsToString : SectorCoordinates -> String
sectorCoordsToString ( x, y ) =
    "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


orientationToString : Orientation -> String
orientationToString orientation =
    case orientation of
        Orientation.North ->
            "↑ N"

        Orientation.East ->
            "→ E"

        Orientation.South ->
            "↓ S"

        Orientation.West ->
            "← W"


viewLevelSettingsTool state =
    Html.div []
        [ Html.div []
            [ Html.div [] [ Html.span [] [ Html.text "Player start position:" ] ]
            , Html.div []
                [ Html.span [ Attr.style "font-family" "monospace" ]
                    [ Html.text
                        (sectorCoordsToString state.playerStartPosition)
                    ]
                , viewOrientationSelect ChangePlayerStartingOrientation state.playerStartingOrientation
                , Html.button [ Attr.classList [ ( "active", state.selectingPlayerPosition ) ], Html.Events.onClick TogglePlayerPositionSelection ] [ Html.text "⌖" ]
                ]
            ]
        ]


viewOrientationSelect msg value =
    Html.select
        [ Html.Events.onInput
            (\val ->
                msg
                    (case val of
                        "N" ->
                            Orientation.North

                        "E" ->
                            Orientation.East

                        "S" ->
                            Orientation.South

                        "W" ->
                            Orientation.West

                        _ ->
                            value
                    )
            )
        ]
        [ Html.option [ Attr.value "N", Attr.selected (value == Orientation.North) ] [ Html.text "↑ N" ]
        , Html.option [ Attr.value "E", Attr.selected (value == Orientation.East) ] [ Html.text "→ E" ]
        , Html.option [ Attr.value "S", Attr.selected (value == Orientation.South) ] [ Html.text "↓ S" ]
        , Html.option [ Attr.value "W", Attr.selected (value == Orientation.West) ] [ Html.text "← W" ]
        ]


viewTilePainterTool state =
    Html.div []
        [ Html.h2 [ Attr.style "font-size" "20px" ] [ Html.text "Tile to place:" ]
        , LevelTile.viewConfig state.currentTile
            |> Html.map TileConfigMsg
        ]


type TriggerConditionMsg
    = ChangeConditionType String
    | SelectSector
    | ChangeOrientation Orientation
    | StringUpdate String
    | IntUpdate Int


viewTriggerCondition : Trigger.TriggerCondition -> Html TriggerConditionMsg
viewTriggerCondition cond =
    Html.div []
        (viewTriggerConditionType cond
            :: (case cond of
                    Trigger.InSector sector ->
                        [ Html.button [ Html.Events.onClick SelectSector ] [ Html.text ("⌖ " ++ sectorCoordsToString sector) ] ]

                    Trigger.EnteredFrom orientation ->
                        [ viewOrientationSelect ChangeOrientation orientation ]

                    Trigger.LookAngle orientation ->
                        [ viewOrientationSelect ChangeOrientation orientation ]

                    Trigger.CounterEquals counterName value ->
                        [ Html.input [ Attr.value counterName, Html.Events.onInput StringUpdate ] []
                        , Html.text " = "
                        , Html.input
                            [ Attr.type_ "number"
                            , Attr.value (String.fromInt value)
                            , Html.Events.onInput (String.toInt >> Maybe.withDefault 0 >> IntUpdate)
                            ]
                            []
                        ]

                    Trigger.SignTextLike signSector textLike ->
                        [ Html.button [ Html.Events.onClick SelectSector ] [ Html.text ("⌖ " ++ sectorCoordsToString signSector) ]
                        , Html.text " has text like "
                        , Html.input [ Attr.value textLike, Html.Events.onInput StringUpdate ] []
                        ]

                    _ ->
                        []
               )
        )


viewTriggerConditionType : Trigger.TriggerCondition -> Html TriggerConditionMsg
viewTriggerConditionType selected =
    let
        selectedValue =
            triggerConditionTypeValue selected

        option ( value, label ) =
            Html.option [ Attr.value value, Attr.selected (selectedValue == value) ] [ Html.text label ]
    in
    Html.select
        [ Html.Events.onInput ChangeConditionType ]
        (List.map option
            [ ( "InSector", "In sector" )
            , ( "SteppedIn", "Stepped in" )
            , ( "EnteredFrom", "Entered from" )
            , ( "LookAngle", "Look angle" )
            , ( "LookingAtGround", "Looking at ground" )
            , ( "NegativeHeadshake", "Negative headshake" )
            , ( "Nod", "Nod" )
            , ( "InSafeTeleportingOffset", "In safe teleporting offset" )
            , ( "CameBackToFloor", "Came back to floor (from ceiling)" )
            , ( "CounterEquals", "Counter" )
            , ( "WindowShake", "Window shaken" )
            , ( "SignTextLike", "Sign at" )
            , ( "CtrlZPressed", "Ctrl+Z pressed" )
            ]
        )


triggerConditionTypeValue : Trigger.TriggerCondition -> String
triggerConditionTypeValue cond =
    case cond of
        Trigger.InSector _ ->
            "InSector"

        Trigger.EnteredFrom _ ->
            "EnteredFrom"

        Trigger.LookAngle _ ->
            "LookAngle"

        Trigger.LookingAtGround ->
            "LookingAtGround"

        Trigger.NegativeHeadshake ->
            "NegativeHeadshake"

        Trigger.Nod ->
            "Nod"

        Trigger.SteppedIn ->
            "SteppedIn"

        Trigger.InSafeTeleportingOffset ->
            "InSafeTeleportingOffset"

        Trigger.CameBackToFloor ->
            "CameBackToFloor"

        Trigger.CounterEquals _ _ ->
            "CounterEquals"

        Trigger.WindowShake ->
            "WindowShake"

        Trigger.SignTextLike _ _ ->
            "SignTextLike"

        Trigger.CtrlZPressed ->
            "CtrlZPressed"

        Trigger.LevelLoaded ->
            "LevelLoaded"


type TriggerEffectMsg
    = ChangeTriggerEffectType String



--viewTriggerEffect : Trigger.TriggerEffect -> Html TriggerEffectMsg
--viewTriggerEffect


viewTriggerManagerTool state =
    let
        globalRows =
            state.globalTriggers
                |> List.indexedMap
                    (\index { conditions, effects } ->
                        Html.tr []
                            [ Html.td [] (List.map (viewTriggerCondition >> Html.map (UpdateTriggerCondition Nothing index)) conditions)
                            ]
                    )

        --localRows =
    in
    Html.div []
        [ Html.table []
            []
        ]


viewCameraTool state =
    Html.div []
        [ Html.div []
            [ Html.div []
                [ Html.span [] [ Html.text "Camera mode" ]
                ]
            , Html.div []
                [ Html.select []
                    [ Html.option [] [ Html.text "Isometric" ]
                    , Html.option [] [ Html.text "Top-down" ]
                    , Html.option [] [ Html.text "First person perspective" ]
                    ]
                ]
            ]
        , Html.div []
            [ Html.div []
                [ Html.span [] [ Html.text "Zoom level" ]
                ]
            , Html.div []
                [ Html.button [ Html.Events.onClick (ZoomChange 2) ] [ Html.text "-" ]
                , Html.button [ Html.Events.onClick (ZoomChange 0.5) ] [ Html.text "+" ]
                ]
            ]
        ]


subscription : Sub Msg
subscription =
    Sub.batch
        [ Browser.Events.onResize WindowResize
        , Browser.Events.onMouseMove
            (D.map2
                MouseMove
                (D.field "x" D.int)
                (D.field "y" D.int)
            )
        , Browser.Events.onMouseDown (canvasMouseEventDecoder MouseDown)
        , Browser.Events.onMouseUp (D.succeed MouseUp)
        , Browser.Events.onKeyDown keyToMsgDecoder
        , Assets.subscription
            |> Sub.map AssetsMsg
        ]


keyToMsgDecoder : D.Decoder Msg
keyToMsgDecoder =
    D.map2 Tuple.pair
        (D.at [ "target", "nodeName" ] D.string)
        (D.field "key" D.string)
        |> D.andThen
            (\( targetTag, key ) ->
                if List.member targetTag [ "INPUT", "TEXTAREA" ] then
                    D.fail "Ignoring hotkeys on input"

                else
                    case String.toUpper key of
                        "S" ->
                            D.succeed (SelectTool LevelSettings)

                        "C" ->
                            D.succeed (SelectTool Camera)

                        "P" ->
                            D.succeed (SelectTool TilePainter)

                        "T" ->
                            D.succeed (SelectTool TriggersManager)

                        _ ->
                            D.fail "Unknown key"
            )


canvasMouseEventDecoder : (Int -> Int -> msg) -> D.Decoder msg
canvasMouseEventDecoder msg =
    D.at [ "target", "nodeName" ] D.string
        |> D.andThen
            (\nodeName ->
                if nodeName == "CANVAS" then
                    D.map2
                        msg
                        (D.field "x" D.int)
                        (D.field "y" D.int)

                else
                    D.fail "Non-canvas event"
            )



--normalizeSectorCoordinates : Dict SectorCoordinates a -> Dict SectorCoordinates a
--normalizeSectorCoordinates dict =
--    let
--
--    in
