module Menu exposing (..)

import Textures
import Scene3d
import Scene3d.Material
import Length
import Point3d
import Luminance
import Html exposing (Html)
import Html.Attributes exposing (class)
import Direction3d
import Color
import Pixels
import Camera3d
import Viewpoint3d
import Angle
import Browser.Events


type alias Model =
    { textures: Textures.Model
    , canvasSize : (Int, Int)
    , offset: Float
    }

type Msg
    = AnimationTick Float
    | WindowResize Int Int

init : Textures.Model -> Model
init textures =
    { textures = textures
    , canvasSize = (800, 600)
    , offset = 0
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationTick delta ->
            let
                newOffset = model.offset + delta * 0.0001
                safeNewOffset = newOffset - toFloat (floor newOffset)
            in
            ({ model | offset = safeNewOffset }, Cmd.none)

        WindowResize width height ->
            ({ model | canvasSize = (width, height) }, Cmd.none)

view : Model -> Html msg
view model =
    let
        segments = List.range 0 40
            |> List.map (\index ->
                let
                    currentSegmentOffset = -(toFloat index) + model.offset
                in
                Scene3d.group
                    [ viewFloor model.textures 0 currentSegmentOffset 0.5
                    , viewWall model.textures 0 currentSegmentOffset 0.5
                    ]
                )
            |> Scene3d.group
    in
        Html.div [class "mainMenuContainer"]
            [ Scene3d.cloudy
                 { entities = [ segments ]
                 , camera = camera
                 , upDirection = Direction3d.z
                 , background = Scene3d.backgroundColor Color.black
                 , clipDepth = Length.centimeters 1
                 , dimensions = Tuple.mapBoth Pixels.int Pixels.int model.canvasSize
                 }
            , Html.div [class "mainMenu"]
                [ Html.div [class "logo"] []
                , Html.div [class "menuPosition"] [Html.text "New game"]
                , Html.div [class "menuPosition"] [Html.text "Settings"]
                , Html.div [class "menuPosition"] [Html.text "About"]
                ]
            ]

camera = Camera3d.perspective
   { viewpoint =
     Viewpoint3d.lookAt
         { eyePoint = Point3d.xyz (Length.meters 0) (Length.meters 0) (Length.meters 0.5)
         , focalPoint = Point3d.xyz (Length.meters 0) (Length.meters -1) (Length.meters 0.5)
         , upDirection = Direction3d.positiveZ
         }
   , verticalFieldOfView = Angle.degrees 110
   }

viewWall textures x y z =
    Maybe.withDefault Scene3d.nothing <|
    Maybe.map2
        (\texture roughness ->
            let
                material = (Scene3d.Material.texturedNonmetal { baseColor = texture, roughness = roughness })
                x1 = Length.meters (-x + 0.5)
                x2 = Length.meters (-x - 0.5)
                y1 = Length.meters (y - 0.5)
                y2 = Length.meters (y + 0.5)
                z1 = Length.meters (z - 0.5)
                z2 = Length.meters (z + 0.5)
            in
                 Scene3d.group
                    [ Scene3d.quad material
                         (Point3d.xyz x1 y1 z1)
                         (Point3d.xyz x1 y2 z1)
                         (Point3d.xyz x1 y2 z2)
                         (Point3d.xyz x1 y1 z2)
                    , Scene3d.quad material
                         (Point3d.xyz x2 y2 z1)
                         (Point3d.xyz x2 y1 z1)
                         (Point3d.xyz x2 y1 z2)
                         (Point3d.xyz x2 y2 z2)
                    ]
        )
    (Textures.getTexture textures "Bricks021_1K-JPG_Color.jpg")
    (Textures.getTextureFloat textures "Bricks021_1K-JPG_Roughness.jpg")

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