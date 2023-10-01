module Main exposing (..)

import Game
import Browser
import Task
import Textures exposing (Model, TextureToLoad(..), TexturesState(..))
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
    = Initialising Textures.Model
    | InitError
    | Playing Game.Model

texturesToLoad =
    [ TextureColor "Bricks021_1K-JPG_Color.jpg"
    , TextureFloat "Bricks021_1K-JPG_Roughness.jpg"
    , TextureColor "CheckerFloor.jpg"
    , TextureColor "OfficeCeiling005_4K_Color.jpg"
    , GenerateSign "Sign-ConfusingCorridor" "BEWARE OF\nCONFUSING\nCORRIDORS"
    , TextureColor "CorrugatedSteel007B_1K-JPG_Color.jpg"
    , TextureFloat "CorrugatedSteel007B_1K-JPG_Metalness.jpg"
    , TextureFloat "CorrugatedSteel007B_1K-JPG_Roughness.jpg"
    , GenerateSign "Sign-Quiz" "QUIZ TIME!\n\nDo you think that there is a\ndead end around the corner?"
    , GenerateSign "Sign-CorrectAnswer" "Correct answer!\n\nIt seems that you were right!"
    , GenerateSign "Sign-CountTo3" "Let's count to 3"
    , GenerateSign "Sign-1" "1"
    , GenerateSign "Sign-2" "2"
    , GenerateSign "Sign-3" "3"
    , GenerateSign "Sign-ProgrammerZero" "Actually,\na good programmer\nwould start with zero...\nso let's start over..."
    , GenerateSign "Sign-JustKiddin" "Just kidding"
    , GenerateSign "Sign-Moonwalk" "MOONWALKERS ONLY"
    , GenerateSign "Sign-Minus1" "-1"
    --, (TextureColor, "Chip004_1K_Color.jpg")
    --, (TextureFloat, "Chip004_1K_Metalness.jpg")
    --, (TextureFloat, "Chip004_1K_Roughness.jpg")
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
    let
        (texturesModel, texturesCmd) = Textures.init texturesToLoad
    in
        ( Initialising texturesModel
        , Cmd.map TexturesMsg texturesCmd
        )


-- UPDATE

type Msg
    = GameMsg Game.Msg
    | TexturesMsg Textures.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (GameMsg gameMsg, Playing gameModel) ->
            (Game.update gameMsg gameModel)
                |> Tuple.mapFirst Playing
                |> Tuple.mapSecond (Cmd.map GameMsg)
        (TexturesMsg texturesMsg, Initialising texturesModel) ->
            let
                (newTexturesModel, texturesCmd) = Textures.update texturesMsg texturesModel
            in
                case Textures.getState newTexturesModel of
                    TexturesLoaded -> (Playing (Game.init newTexturesModel), Cmd.none)
                    RemainingTextures _ -> (Initialising newTexturesModel, Cmd.map TexturesMsg texturesCmd)
                    TextureInitError -> (InitError, Cmd.none)

        _ -> (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing game ->
            Game.subscriptions game
                |> Sub.map GameMsg
        Initialising _ ->
            Textures.subscription
                |> Sub.map TexturesMsg
        _ -> Sub.none

view : Model -> Html Msg
view model =
    case model of
        Playing game ->
            Game.view game
                |> Html.map GameMsg
        Initialising _ ->
            Html.div [] [Html.text "Loading..."]
        InitError ->
            Html.div [] [Html.text "Error :-("]