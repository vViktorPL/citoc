module Terms exposing (Model, dependencies, doesCollide, init, open, update, view)

import Assets
import Point3d
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import TriangularMesh
import Vector3d


type Model
    = Closed
    | ShiftingBackAnimation Float
    | OpeningAnimation Float
    | FullyOpened


init : Model
init =
    Closed


dependencies : List Assets.Dependency
dependencies =
    [ Assets.SoundEffectDep "elevator_door.mp3"
    , Assets.OtherTextureDep "toc.png"
    ]


view assets model =
    let
        material =
            Scene3d.Material.texturedColor (Assets.getColorTexture assets "toc.png")

        leftSideOriginal =
            TriangularMesh.triangles
                [ ( { position = Point3d.unsafe { x = 0.5, y = 0, z = 0 }, uv = ( 0, 0 ) }
                  , { position = Point3d.unsafe { x = 0, y = 0, z = 0 }, uv = ( 0.5, 0 ) }
                  , { position = Point3d.unsafe { x = 0, y = 0, z = 1 }, uv = ( 0.5, 1 ) }
                  )
                , ( { position = Point3d.unsafe { x = 0.5, y = 0, z = 0 }, uv = ( 0, 0 ) }
                  , { position = Point3d.unsafe { x = 0, y = 0, z = 1 }, uv = ( 0.5, 1 ) }
                  , { position = Point3d.unsafe { x = 0.5, y = 0, z = 1 }, uv = ( 0, 1 ) }
                  )
                ]
                |> Scene3d.Mesh.texturedTriangles
                |> Scene3d.mesh material

        rightSideOriginal =
            TriangularMesh.triangles
                [ ( { position = Point3d.unsafe { x = 0, y = 0, z = 0 }, uv = ( 0.5, 0 ) }
                  , { position = Point3d.unsafe { x = -0.5, y = 0, z = 0 }, uv = ( 1, 0 ) }
                  , { position = Point3d.unsafe { x = -0.5, y = 0, z = 1 }, uv = ( 1, 1 ) }
                  )
                , ( { position = Point3d.unsafe { x = 0, y = 0, z = 0 }, uv = ( 0.5, 0 ) }
                  , { position = Point3d.unsafe { x = -0.5, y = 0, z = 1 }, uv = ( 1, 1 ) }
                  , { position = Point3d.unsafe { x = 0, y = 0, z = 1 }, uv = ( 0.5, 1 ) }
                  )
                ]
                |> Scene3d.Mesh.texturedTriangles
                |> Scene3d.mesh material

        ( leftSide, rightSide ) =
            case model of
                Closed ->
                    ( leftSideOriginal, rightSideOriginal )

                ShiftingBackAnimation progress ->
                    ( leftSideOriginal
                        |> Scene3d.translateBy (Vector3d.unsafe { x = 0, y = -progress * 0.2, z = 0 })
                    , rightSideOriginal
                        |> Scene3d.translateBy (Vector3d.unsafe { x = 0, y = -progress * 0.2, z = 0 })
                    )

                OpeningAnimation progress ->
                    ( leftSideOriginal
                        |> Scene3d.translateBy (Vector3d.unsafe { x = progress * 0.5, y = -0.2, z = 0 })
                    , rightSideOriginal
                        |> Scene3d.translateBy (Vector3d.unsafe { x = -progress * 0.5, y = -0.2, z = 0 })
                    )

                FullyOpened ->
                    ( leftSideOriginal
                        |> Scene3d.translateBy (Vector3d.unsafe { x = 0.5, y = -0.2, z = 0 })
                    , rightSideOriginal
                        |> Scene3d.translateBy (Vector3d.unsafe { x = -0.5, y = -0.2, z = 0 })
                    )
    in
    Scene3d.group [ leftSide, rightSide ]


update : Float -> Model -> Model
update delta termsState =
    case termsState of
        ShiftingBackAnimation progress ->
            let
                newProgress =
                    progress + delta * 0.001
            in
            if newProgress >= 1 then
                OpeningAnimation 0

            else
                ShiftingBackAnimation newProgress

        OpeningAnimation progress ->
            let
                newProgress =
                    progress + delta * 0.0005
            in
            if newProgress >= 1 then
                FullyOpened

            else
                OpeningAnimation newProgress

        _ ->
            termsState


open : Model -> Model
open model =
    case model of
        Closed ->
            ShiftingBackAnimation 0

        _ ->
            model


doesCollide : Model -> Bool
doesCollide model =
    case model of
        FullyOpened ->
            False

        _ ->
            True
