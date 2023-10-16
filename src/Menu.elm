module Menu exposing (..)

import Textures
import Scene3d
import Scene3d.Material
import Length
import Point3d
import Luminance
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Direction3d
import Color
import Pixels
import Camera3d
import Viewpoint3d
import Angle
import Browser.Events
import Browser.Dom
import Task
import Sound
import SceneAssets
import Frame3d
import Vector3d

type alias Model =
    { initialized : Bool
    , canvasSize : (Int, Int)
    , offset: Float
    , state: MenuState
    }

type Msg
    = AnimationTick Float
    | WindowResize Int Int
    | NewGamePositionActivated

type MenuState
    = Idle
    | StartingNewGame Float

type OutMsg
    = Noop
    | StartNewGame

init : (Model, Cmd Msg)
init =
    ( { initialized = False
      , canvasSize = (800, 600)
      , offset = 0
      , state = Idle
      }
    , Cmd.batch
        [ Task.perform
            (\viewportDetails -> WindowResize (floor viewportDetails.viewport.width) (floor viewportDetails.viewport.height))
            Browser.Dom.getViewport
        , Sound.playMusic "menu.mp3"
        ]
    )

update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        AnimationTick delta ->
            let
                newOffset = model.offset + delta * 0.0001
                safeNewOffset = newOffset - toFloat (floor newOffset)
                animatedModel = { model | offset = safeNewOffset, initialized = True }
            in
                case model.state of
                    Idle -> (animatedModel, Noop)
                    StartingNewGame remainingFadeout ->
                        let
                            newFadeout = max 0 (remainingFadeout - delta * 0.002)
                        in
                            ({ animatedModel | state = StartingNewGame newFadeout }
                            , if newFadeout == 0 then StartNewGame else Noop
                            )

        WindowResize width height ->
            ({ model | canvasSize = (width, height) }, Noop)

        NewGamePositionActivated ->
            ({ model | state = StartingNewGame 1.0 }, Noop)

view : SceneAssets.Model -> Model -> Html Msg
view sceneAssets model =
    let
        opacity = String.fromFloat
            (case model.state of
                StartingNewGame fadeOut -> fadeOut
                _ -> 1
            )
        segments = List.range 0 40
            |> List.map (\index ->
                let
                    currentSegmentOffset = -(toFloat index) + model.offset
                    position = (Point3d.meters 0 currentSegmentOffset 0)
                in
                Scene3d.group
                    [ SceneAssets.floorTile sceneAssets
                    , SceneAssets.ceilingTile sceneAssets
                    , SceneAssets.wallBlock sceneAssets |> Scene3d.translateBy (Vector3d.fromMeters { x = 1, y = 0, z = 0 })
                    , SceneAssets.wallBlock sceneAssets |> Scene3d.translateBy (Vector3d.fromMeters { x = -1, y = 0, z = 0 })
                    ]
                    |> Scene3d.placeIn (Frame3d.atPoint position)
                )
    in
        Html.div [class "mainMenuContainer", style "opacity" opacity]
            [ Html.div [] [Html.text (if model.initialized then "" else "Initializing...")]
            , Scene3d.cloudy
                 { entities = segments
                 , camera = camera
                 , upDirection = Direction3d.z
                 , background = Scene3d.backgroundColor Color.white
                 , clipDepth = Length.centimeters 1
                 , dimensions = Tuple.mapBoth Pixels.int Pixels.int model.canvasSize
                 , visibility = Scene3d.fog (Length.meters 4)
                 }
            , Html.div [class "mainMenu", style "visibility" (if model.initialized then "visible" else "hidden")]
                [ Html.div [class "logo"] []
                , Html.div [class "menuPosition", Html.Events.onClick NewGamePositionActivated ] [Html.text "New game"]
                , Html.div [class "menuPosition", class "disabled"] [Html.text "Settings"]
                , Html.div [class "menuPosition", class "disabled"] [Html.text "About"]
                ]
            ]

camera = Camera3d.perspective
   { viewpoint =
     Viewpoint3d.lookAt
         { eyePoint = Point3d.xyz (Length.meters 0) (Length.meters 0.5) (Length.meters 0.5)
         , focalPoint = Point3d.xyz (Length.meters 0) (Length.meters -1) (Length.meters 0.5)
         , upDirection = Direction3d.positiveZ
         }
   , verticalFieldOfView = Angle.degrees 110
   }
--
--viewWall textures x y z =
--    Maybe.withDefault Scene3d.nothing <|
--    Maybe.map2
--        (\texture roughness ->
--            let
--                material = (Scene3d.Material.texturedNonmetal { baseColor = texture, roughness = roughness })
--                x1 = Length.meters (-x + 0.5)
--                x2 = Length.meters (-x - 0.5)
--                y1 = Length.meters (y - 0.5)
--                y2 = Length.meters (y + 0.5)
--                z1 = Length.meters (z - 0.5)
--                z2 = Length.meters (z + 0.5)
--            in
--                 Scene3d.group
--                    [ Scene3d.quad material
--                         (Point3d.xyz x1 y1 z1)
--                         (Point3d.xyz x1 y2 z1)
--                         (Point3d.xyz x1 y2 z2)
--                         (Point3d.xyz x1 y1 z2)
--                    , Scene3d.quad material
--                         (Point3d.xyz x2 y2 z1)
--                         (Point3d.xyz x2 y1 z1)
--                         (Point3d.xyz x2 y1 z2)
--                         (Point3d.xyz x2 y2 z2)
--                    ]
--        )
--    (Textures.getTexture textures "Bricks021_1K-JPG_Color.jpg")
--    (Textures.getTextureFloat textures "Bricks021_1K-JPG_Roughness.jpg")

viewFloor textures x y z =
    Maybe.withDefault Scene3d.nothing <|
    Maybe.map2
        (\floorTexture ceilingTexture ->
            let
               floorMaterial = (Scene3d.Material.texturedNonmetal { baseColor = floorTexture, roughness = Scene3d.Material.constant 0.5 })
               ceilingMaterial = (Scene3d.Material.texturedEmissive ceilingTexture (Luminance.footLamberts 100 ))
               x1 = Length.meters (-x + 0.5)
               y1 = Length.meters (y + 0.5)

               x2 = Length.meters (-x - 0.5)
               y2 = Length.meters (y + 0.5)
               x3 = Length.meters (-x - 0.5)
               y3 = Length.meters (y - 0.5)
               x4 = Length.meters (-x + 0.5)
               y4 = Length.meters (y - 0.5)

               zBottom = Length.meters (z - 0.5)
               zTop = Length.meters (z + 0.5)
            in
                Scene3d.group
                    [ Scene3d.quad floorMaterial
                         (Point3d.xyz x1 y1 zBottom)
                         (Point3d.xyz x2 y2 zBottom)
                         (Point3d.xyz x3 y3 zBottom)
                         (Point3d.xyz x4 y4 zBottom)
                    , Scene3d.quad ceilingMaterial
                         (Point3d.xyz x1 y1 zTop)
                         (Point3d.xyz x2 y2 zTop)
                         (Point3d.xyz x3 y3 zTop)
                         (Point3d.xyz x4 y4 zTop)
                    ]
        )
        (Textures.getTexture textures "CheckerFloor.jpg")
        (Textures.getTexture textures "OfficeCeiling005_4K_Color.jpg")


subscription : Model -> Sub Msg
subscription menu =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationTick
        , Browser.Events.onResize WindowResize
        ]
