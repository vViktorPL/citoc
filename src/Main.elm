module Main exposing (..)

import Game
import Browser
import Textures exposing (Model, TextureToLoad(..), TexturesState(..))
import Html exposing (Html)
import Menu
import MeshCollection exposing (Model(..))


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model
    = { screen: Screen
      , textures: Textures.Model
      , meshes: MeshCollection.Model
      }

type Screen
    = Initialising
    | InitError
    | InMenu Menu.Model
    | Playing Game.Model

texturesToLoad =
    [ TextureColor "Bricks021_1K-JPG_Color.jpg"
    , TextureFloat "Bricks021_1K-JPG_Roughness.jpg"
    , TextureColor "CheckerFloor.jpg"
    , TextureColor "OfficeCeiling005_4K_Color.jpg"
    , TextureColor "CorrugatedSteel007B_1K-JPG_Color.jpg"
    , TextureFloat "CorrugatedSteel007B_1K-JPG_Metalness.jpg"
    , TextureFloat "CorrugatedSteel007B_1K-JPG_Roughness.jpg"
    , GenerateSign "Sign-ConfusingCorridor" "BEWARE OF\nCONFUSING\nCORRIDORS"
    , GenerateSign "Sign-Quiz" "QUIZ TIME!\n\nDo you think that there is a\ndead end around the corner?"
    , GenerateSign "Sign-CorrectAnswer" "Correct answer!\n\nIt seems that you were right!"
    , GenerateSign "Sign-CountTo3" "Let's count to 3"
    , GenerateSign "Sign-1" "1"
    , GenerateSign "Sign-2" "2"
    , GenerateSign "Sign-3" "3"
    , GenerateSign "Sign-ProgrammerZero" "Actually,\na good programmer\nwould start with zero...\nso let's start over..."
    , GenerateSign "Sign-JustKiddin" "Just kidding"
    , GenerateSign "Sign-Moonwalk" "MOONWALKERS\nONLY"
    , GenerateSign "Sign-Minus1" "-1"
    , TextureColor "ConeColor.jpg"
    --, (TextureColor, "Chip004_1K_Color.jpg")
    --, (TextureFloat, "Chip004_1K_Metalness.jpg")
    --, (TextureFloat, "Chip004_1K_Roughness.jpg")
    ]

meshesToLoad =
    [ "Cone.obj"
    ]

init : (Model, Cmd Msg)
init =
    let
        (texturesModel, texturesCmd) = Textures.init texturesToLoad
        (meshesModel, meshesCmd) = MeshCollection.init meshesToLoad
    in
        ( { screen = Initialising
          , textures = texturesModel
          , meshes = meshesModel
          }
        , Cmd.batch
            [ Cmd.map TexturesMsg texturesCmd
            , Cmd.map MeshesMsg meshesCmd
            ]
        )


-- UPDATE

type Msg
    = GameMsg Game.Msg
    | TexturesMsg Textures.Msg
    | MeshesMsg MeshCollection.Msg
    | MenuMsg Menu.Msg


handleInitializationFinish : ( Model, Cmd Msg) -> ( Model, Cmd Msg )
handleInitializationFinish (model, cmd) =
    case (Textures.getState model.textures, model.meshes) of
        (TexturesLoaded, MeshCollectionLoaded _) ->
            let
                (menuModel, menuCmd) = Menu.init model.textures model.meshes
            in
                ({ model | screen = InMenu menuModel}, Cmd.batch [cmd, Cmd.map MenuMsg menuCmd])

        (TextureInitError, _) ->
            ({ model | screen = InitError }, Cmd.none)

        (_, MeshCollectionFailed) ->
            ({ model | screen = InitError }, Cmd.none)

        _ -> (model, cmd)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model.screen) of
        (GameMsg gameMsg, Playing gameModel) ->
            let
                (newGameModel, gameCmd) = (Game.update gameMsg gameModel)
            in
                ({ model | screen = Playing newGameModel }, Cmd.map GameMsg gameCmd)

        (MeshesMsg meshesMsg, Initialising) ->
            let
                (newMeshesModel, meshesCmd) = MeshCollection.update meshesMsg model.meshes
            in
                handleInitializationFinish ({ model | meshes = newMeshesModel }, Cmd.map MeshesMsg meshesCmd)

        (TexturesMsg texturesMsg, Initialising) ->
            let
                (newTexturesModel, texturesCmd) = Textures.update texturesMsg model.textures
            in
                handleInitializationFinish ({ model | textures = newTexturesModel }, Cmd.map TexturesMsg texturesCmd)

        (MenuMsg menuMsg, InMenu menuModel) ->
            let
                (newMenuModel, menuOutMsg) = Menu.update menuMsg menuModel
            in
                case menuOutMsg of
                    Menu.Noop -> ({ model | screen = InMenu newMenuModel }, Cmd.none)
                    Menu.StartNewGame ->
                        let
                            (initializedGameModel, gameCmd) = (Game.init model.textures)
                        in
                        ({ model | screen = Playing initializedGameModel }, Cmd.map GameMsg gameCmd )

        _ -> (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        Playing game ->
            Game.subscriptions game
                |> Sub.map GameMsg
        Initialising ->
            Textures.subscription
                |> Sub.map TexturesMsg
        InMenu menu ->
            Menu.subscription menu
                |> Sub.map MenuMsg
        _ -> Sub.none

view : Model -> Html Msg
view model =
    case model.screen of
        Playing game ->
            Game.view game
                |> Html.map GameMsg
        Initialising ->
            Html.div [] [Html.text "Loading..."]
        InitError ->
            Html.div [] [Html.text "Error :-("]
        InMenu menu ->
            Menu.view menu
                |> Html.map MenuMsg