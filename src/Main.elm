module Main exposing (..)

import Assets
import Browser
import Browser.Events
import Game
import Html exposing (Html)
import Html.Attributes
import Json.Decode as D
import Menu
import Settings


main : Program Settings.InitFlags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { screen : Screen
    , assets : Assets.Model
    , settings : Settings.Model
    }


type Screen
    = Initialising
    | InitError
    | InMenu Menu.Model
    | Playing Game.Model


init : Settings.InitFlags -> ( Model, Cmd Msg )
init flags =
    let
        ( assets, sceneAssetsCmd ) =
            Assets.init
                |> Assets.requestDependencies Menu.dependencies
    in
    ( { screen = Initialising
      , assets = assets
      , settings = Settings.init flags
      }
    , Cmd.map AssetsMsg sceneAssetsCmd
    )


type Msg
    = GameMsg Game.Msg
    | AssetsMsg Assets.Msg
    | MenuMsg Menu.Msg
    | SettingsMsg Settings.Msg
    | EscapeKeyPressed


showGameCompletedMenu : Model -> ( Model, Cmd Msg )
showGameCompletedMenu model =
    let
        ( menu, menuCmd ) =
            Menu.init True
    in
    ( { model | screen = InMenu menu }, menuCmd |> Cmd.map MenuMsg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.screen ) of
        ( GameMsg gameMsg, Playing gameModel ) ->
            if Game.isGameCompletedMsg gameMsg then
                showGameCompletedMenu model

            else
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
                        Menu.init False
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
                            Game.init model.assets model.settings
                    in
                    ( { model | screen = Playing initializedGameModel }, Cmd.map GameMsg gameCmd )

                Menu.ShowSettings ->
                    ( { model | settings = Settings.show model.settings }, Cmd.none )

        ( SettingsMsg settingsMsg, screen ) ->
            let
                ( updatedSettings, settingsCmd ) =
                    Settings.update settingsMsg model.settings
            in
            ( { model
                | settings = updatedSettings
                , screen =
                    case screen of
                        Playing game ->
                            Playing (Game.updateSettings game updatedSettings)

                        _ ->
                            screen
              }
            , Cmd.map SettingsMsg settingsCmd
            )

        ( EscapeKeyPressed, Playing _ ) ->
            ( { model | settings = Settings.show model.settings }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        case key of
                            "Escape" ->
                                D.succeed EscapeKeyPressed

                            _ ->
                                D.fail "Unknown key"
                    )
            )
        , case model.screen of
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
        ]


view : Model -> Html Msg
view model =
    let
        settingsExtraMessage =
            case model.screen of
                InMenu _ ->
                    "(you can open settings at any time during the game by pressing the \"Escape\" button)"

                _ ->
                    ""
    in
    Html.div []
        [ Settings.view settingsExtraMessage model.settings |> Html.map SettingsMsg
        , case model.screen of
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
        ]
