module Main exposing (..)

import Game
import Browser
import Task
import Textures exposing (Textures, TextureType(..))
import Html exposing (Html)


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

type Model
    = Initialising
    | InitError
    | Playing Game.Model

texturesToLoad =
    [ (TextureColor, "Bricks021_1K-JPG_Color.jpg")
    , (TextureFloat, "Bricks021_1K-JPG_Roughness.jpg")
    , (TextureColor, "CheckerFloor.jpg")
    , (TextureColor, "OfficeCeiling005_4K_Color.jpg")
    , (TextureColor, "Sign-ConfusingCorridor.png")
    , (TextureColor, "CorrugatedSteel007B_1K-JPG_Color.jpg")
    , (TextureFloat, "CorrugatedSteel007B_1K-JPG_Metalness.jpg")
    , (TextureFloat, "CorrugatedSteel007B_1K-JPG_Roughness.jpg")
    --, "Text-Long.png"
    --, "Sign1.png"
    --, "Sign2.png"
    --, "Sign3.png"
    --, "Sign4.png"
    --, "Sign5.png"
    --, "Sign6.png"
    --, "Sign7.png"
    --, "Sign8.png"
    ]

init : (Model, Cmd Msg)
init =
    (Initialising
    , Task.attempt
        (\result ->
            case result of
                Ok textures -> Loaded textures
                Err _ -> LoadingFailed
        )
        (Textures.loadTextures texturesToLoad)
    )


-- UPDATE

type Msg
    = Loaded Textures
    | LoadingFailed
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (Loaded textures, _) -> (Playing (Game.init textures), Cmd.none)
        (LoadingFailed, _) -> (InitError, Cmd.none)
        (GameMsg gameMsg, Playing gameModel) ->
            (Game.update gameMsg gameModel)
                |> Tuple.mapFirst Playing
                |> Tuple.mapSecond (Cmd.map GameMsg)
        _ -> (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing game ->
            Game.subscriptions game
                |> Sub.map GameMsg
        _ -> Sub.none

view : Model -> Html Msg
view model =
    case model of
        Playing game ->
            Game.view game
                |> Html.map GameMsg
        Initialising ->
            Html.div [] [Html.text "Loading..."]
        InitError ->
            Html.div [] [Html.text "Error :-("]