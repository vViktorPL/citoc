module Main exposing (..)

import Assets
import Browser
import Game
import Html exposing (Html)
import Html.Attributes
import Menu


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { screen : Screen
    , assets : Assets.Model
    }


type Screen
    = Initialising
    | InitError
    | InMenu Menu.Model
    | Playing Game.Model


init : ( Model, Cmd Msg )
init =
    let
        ( assets, sceneAssetsCmd ) =
            Assets.init
                |> Assets.requestDependencies Menu.dependencies
    in
    ( { screen = Initialising
      , assets = assets
      }
    , Cmd.map AssetsMsg sceneAssetsCmd
    )


type Msg
    = GameMsg Game.Msg
    | AssetsMsg Assets.Msg
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

        ( AssetsMsg sceneAssetsMsg, Initialising ) ->
            let
                ( newSceneAssetsModel, sceneAssetsCmd ) =
                    Assets.update sceneAssetsMsg model.assets

                assetsReady =
                    Assets.areReady newSceneAssetsModel

                assetsUpdatedModel =
                    { model | assets = newSceneAssetsModel }
            in
            if assetsReady then
                let
                    ( menuModel, menuCmd ) =
                        Menu.init
                in
                ( { assetsUpdatedModel | screen = InMenu menuModel }, Cmd.map MenuMsg menuCmd )

            else
                ( assetsUpdatedModel, Cmd.map AssetsMsg sceneAssetsCmd )

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
                            Game.init model.assets
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
            Assets.subscription
                |> Sub.map AssetsMsg

        InMenu menu ->
            Menu.subscription menu
                |> Sub.map MenuMsg

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model.screen of
        Playing game ->
            Game.view game
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
            Menu.view model.assets menu
                |> Html.map MenuMsg
