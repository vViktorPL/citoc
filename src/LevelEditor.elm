module LevelEditor exposing (Model, Msg, init, subscription, update, view)

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
    , windowSize : ( Int, Int )
    , selectedSector : SectorCoordinates
    , isMouseDown : Bool
    , mouseDownAtPoint : Maybe (Point3d Meters EditorWorldCoordinates)
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
                    let
                        sectorX =
                            intersectionPoint
                                |> Point3d.xCoordinate
                                |> Length.inMeters
                                |> ceiling
                                |> (\n -> -n)

                        sectorY =
                            intersectionPoint
                                |> Point3d.yCoordinate
                                |> Length.inMeters
                                |> floor
                    in
                    ( { state | selectedSector = ( sectorX, sectorY ) }
                        |> (if state.isMouseDown then
                                placeTile

                            else
                                identity
                           )
                        |> LevelEditor
                    , Cmd.none
                    )

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


view : Model -> Html Msg
view (LevelEditor state) =
    let
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
            , ( "crosshair", state.selectedTool == TilePainter )
            ]
        ]
        [ Scene3d.cloudy
            { entities =
                [ tileSelectionEntity
                    |> Scene3d.placeIn (Frame3d.atPoint (Coordinates.sectorToEditorWorldPosition state.selectedSector))
                ]
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


viewTilePainterTool state =
    Html.div []
        [ Html.h2 [ Attr.style "font-size" "20px" ] [ Html.text "Tile to place:" ]
        , LevelTile.viewConfig state.currentTile
            |> Html.map TileConfigMsg
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
