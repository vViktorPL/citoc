module Main exposing (..)

import Assets
import Browser
import Browser.Events
import Game
import Html exposing (Html)
import Html.Attributes
import Json.Decode as D
import LevelEditor
import Menu
import Settings


type alias Flags =
    { save : Maybe Game.SavedGameState
    , settings : Settings.InitFlags
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { screen : Screen
    , savedGame : Game.SavedGameState
    , assets : Assets.Model
    , settings : Settings.Model
    }


type Screen
    = Initialising
    | InitError
    | InMenu Menu.Model
    | Playing Game.Model
    | InEditor LevelEditor.Model


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( assets, sceneAssetsCmd ) =
            Assets.init
                |> Assets.requestDependencies Menu.dependencies

        ( levelEditor, levelEditorCmd ) =
            LevelEditor.init ( 800, 600 )
    in
    ( { screen = InEditor levelEditor
      , savedGame = Maybe.withDefault Game.newGameState flags.save
      , assets = assets
      , settings = Settings.init flags.settings
      }
    , Cmd.map EditorMsg levelEditorCmd
      --Cmd.map AssetsMsg sceneAssetsCmd
    )


type Msg
    = GameMsg Game.Msg
    | AssetsMsg Assets.Msg
    | MenuMsg Menu.Msg
    | SettingsMsg Settings.Msg
    | EditorMsg LevelEditor.Msg
    | EscapeKeyPressed


showGameCompletedMenu : Model -> ( Model, Cmd Msg )
showGameCompletedMenu model =
    let
        ( menu, menuCmd ) =
            Menu.init { alternativeMenu = True, continuationAvailable = False }
    in
    ( { model | screen = InMenu menu, savedGame = Game.newGameState }
    , Cmd.batch
        [ menuCmd |> Cmd.map MenuMsg
        , Game.deleteSavedGame
        ]
    )


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
                        Menu.init { alternativeMenu = False, continuationAvailable = not (Game.isNewGameState model.savedGame) }
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
                        gameState =
                            Game.newGameState

                        ( initializedGameModel, gameCmd ) =
                            Game.init model.assets model.settings gameState
                    in
                    ( { model | screen = Playing initializedGameModel, savedGame = gameState }, Cmd.batch [ Game.saveGame initializedGameModel, Cmd.map GameMsg gameCmd ] )

                Menu.ContinueGame ->
                    let
                        ( initializedGameModel, gameCmd ) =
                            Game.init model.assets model.settings model.savedGame
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

        ( EditorMsg editorMsg, InEditor editor ) ->
            let
                ( updatedEditor, editorCmd ) =
                    LevelEditor.update editorMsg editor
            in
            ( { model | screen = InEditor updatedEditor }, Cmd.map EditorMsg editorCmd )

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

            InEditor _ ->
                LevelEditor.subscription
                    |> Sub.map EditorMsg

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

            InEditor editor ->
                LevelEditor.view editor
                    |> Html.map EditorMsg
        ]
