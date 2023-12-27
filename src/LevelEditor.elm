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


type alias EditorState =
    { assets : Assets.Model
    , tiles : Dict SectorCoordinates ( LevelTile.Model, List Trigger.Trigger )
    , currentTile : LevelTile.Model
    , triggers : List Trigger.Trigger
    , globalTriggers : List Trigger.Trigger
    , zoom : Length
    , focalPoint : Point3d Meters EditorWorldCoordinates
    , windowSize : ( Int, Int )
    , selectedSector : SectorCoordinates
    , paintingTiles : Bool
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
    | MouseDown
    | MouseUp
    | TileConfigMsg LevelTile.ConfigMsg


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
        , tiles = Dict.fromList [ ( ( 0, 0 ), ( LevelTile.wall, [] ) ), ( ( 0, 1 ), ( LevelTile.floor, [] ) ) ]
        , currentTile = LevelTile.wall
        , globalTriggers = []
        , triggers = []
        , zoom = Length.meters 10
        , focalPoint = Point3d.meters 2 7 0
        , selectedSector = ( 0, 0 )
        , paintingTiles = False
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (LevelEditor state) =
    case msg of
        AssetsMsg assetsMsg ->
            let
                ( updatedAssets, assetsCmd ) =
                    Assets.update assetsMsg state.assets
            in
            ( LevelEditor { state | assets = updatedAssets }, Cmd.map AssetsMsg assetsCmd )

        WindowResize width height ->
            ( LevelEditor { state | windowSize = ( width, height ) }, Cmd.none )

        MouseDown ->
            let
                requiredDeps =
                    LevelTile.dependencies state.currentTile

                ( updatedAssets, assetsCmd ) =
                    Assets.requestDependencies requiredDeps state.assets
            in
            ( LevelEditor (placeTile { state | paintingTiles = True, assets = updatedAssets }), assetsCmd |> Cmd.map AssetsMsg )

        MouseUp ->
            ( LevelEditor { state | paintingTiles = False }, Cmd.none )

        MouseMove x y ->
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

                maybeIntersectionPoint =
                    Axis3d.intersectionWithPlane Plane3d.xy ray
            in
            case maybeIntersectionPoint of
                Just intersectionPoint ->
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
                        |> (if state.paintingTiles then
                                placeTile

                            else
                                identity
                           )
                        |> LevelEditor
                    , Cmd.none
                    )

                Nothing ->
                    ( LevelEditor state, Cmd.none )

        TileConfigMsg tileConfigMsg ->
            ( LevelEditor { state | currentTile = LevelTile.updateConfig tileConfigMsg state.currentTile }, Cmd.none )

        _ ->
            ( LevelEditor state, Cmd.none )


camera : EditorState -> Camera3d Meters EditorWorldCoordinates
camera { zoom, focalPoint } =
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.isometric
                { focalPoint = focalPoint
                , distance = zoom
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
                        LevelTile.view state.assets tile
                            |> Scene3d.placeIn (Frame3d.atPoint (Coordinates.sectorToEditorWorldPosition sector))
                    )
    in
    Html.div [ Attr.id "editor", Attr.style "position" "relative" ]
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
            [ Attr.style "position" "fixed"
            , Attr.style "right" "0"
            , Attr.style "top" "0"
            , Attr.style "height" "100vh"
            , Attr.style "min-width" "150px"
            , Attr.style "width" "20vw"
            , Attr.style "background" "rgba(128, 128, 128, 0.8)"
            ]
            [ Html.h2 [ Attr.style "font-size" "20px" ] [ Html.text "Tile to place:" ]
            , LevelTile.viewConfig state.currentTile
                |> Html.map TileConfigMsg
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
        , Browser.Events.onMouseDown (canvasEventDecoder MouseDown)
        , Browser.Events.onMouseUp (D.succeed MouseUp)
        , Assets.subscription
            |> Sub.map AssetsMsg
        ]


canvasEventDecoder : msg -> D.Decoder msg
canvasEventDecoder msg =
    D.at [ "target", "nodeName" ] D.string
        |> D.andThen
            (\nodeName ->
                if nodeName == "CANVAS" then
                    D.succeed msg

                else
                    D.fail "Non-canvas event"
            )
