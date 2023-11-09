module Main exposing (..)

import Browser
import Game
import Html exposing (Html)
import Html.Attributes
import Menu
import SceneAssets



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


type alias Model =
    { screen : Screen
    , sceneAssets : SceneAssets.Model
    }


type Screen
    = Initialising
    | InitError
    | InMenu Menu.Model
    | Playing Game.Model


init : ( Model, Cmd Msg )
init =
    let
        ( sceneAssetsModel, sceneAssetsCmd ) =
            SceneAssets.init
    in
    ( { screen = Initialising
      , sceneAssets = sceneAssetsModel
      }
    , Cmd.map SceneAssetsMsg sceneAssetsCmd
    )



-- UPDATE


type Msg
    = GameMsg Game.Msg
    | SceneAssetsMsg SceneAssets.Msg
    | MenuMsg Menu.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.screen ) of
        ( GameMsg gameMsg, Playing gameModel ) ->
            let
                ( newGameModel, gameCmd ) =
                    Game.update gameMsg gameModel
            in
            ( { model | screen = Playing newGameModel }, Cmd.map GameMsg gameCmd )

        ( SceneAssetsMsg sceneAssetsMsg, Initialising ) ->
            let
                ( newSceneAssetsModel, sceneAssetsCmd ) =
                    SceneAssets.update sceneAssetsMsg model.sceneAssets

                assetsReady =
                    SceneAssets.ready newSceneAssetsModel

                assetsUpdatedModel =
                    { model | sceneAssets = newSceneAssetsModel }
            in
            if assetsReady then
                let
                    ( menuModel, menuCmd ) =
                        Menu.init
                in
                ( { assetsUpdatedModel | screen = InMenu menuModel }, Cmd.map MenuMsg menuCmd )

            else
                ( assetsUpdatedModel, Cmd.map SceneAssetsMsg sceneAssetsCmd )

        ( MenuMsg menuMsg, InMenu menuModel ) ->
            let
                ( newMenuModel, menuOutMsg ) =
                    Menu.update menuMsg menuModel
            in
            case menuOutMsg of
                Menu.Noop ->
                    ( { model | screen = InMenu newMenuModel }, Cmd.none )

                Menu.StartNewGame ->
                    let
                        ( initializedGameModel, gameCmd ) =
                            Game.init
                    in
                    ( { model | screen = Playing initializedGameModel }, Cmd.map GameMsg gameCmd )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        Playing game ->
            Game.subscriptions game
                |> Sub.map GameMsg

        Initialising ->
            SceneAssets.subscription model.sceneAssets
                |> Sub.map SceneAssetsMsg

        InMenu menu ->
            Menu.subscription menu
                |> Sub.map MenuMsg

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model.screen of
        Playing game ->
            Game.view model.sceneAssets game
                |> Html.map GameMsg

        Initialising ->
            Html.div
                [ Html.Attributes.class "loadingScreen" ]
                [ Html.div [ Html.Attributes.class "loadingSpinner" ] []
                , Html.text "Loading..."
                ]

        InitError ->
            Html.div [] [ Html.text "Error :-(" ]

        InMenu menu ->
            Menu.view model.sceneAssets menu
                |> Html.map MenuMsg
