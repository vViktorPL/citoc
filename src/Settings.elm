port module Settings exposing (InitFlags, Model, Msg, init, show, update, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as D


port setSoundVolume : Float -> Cmd msg


port setMusicVolume : Float -> Cmd msg


type alias InitFlags =
    { mouseSensitivity : Float
    , musicVolume : Float
    , soundVolume : Float
    }


type alias Model =
    { mouseSensitivity : Float
    , musicVolume : Float
    , soundVolume : Float
    , visible : Bool
    }


type Msg
    = MouseSensitivityUpdated Float
    | MusicVolumeUpdated Float
    | SoundVolumeUpdated Float
    | CloseSettings


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseSensitivityUpdated newSensitivity ->
            ( { model | mouseSensitivity = newSensitivity }, Cmd.none )

        MusicVolumeUpdated newVolume ->
            ( { model | musicVolume = newVolume }, setMusicVolume newVolume )

        SoundVolumeUpdated newVolume ->
            ( { model | soundVolume = newVolume }, setSoundVolume newVolume )

        CloseSettings ->
            ( { model | visible = False }, Cmd.none )


init : InitFlags -> Model
init { mouseSensitivity, musicVolume, soundVolume } =
    { mouseSensitivity = mouseSensitivity
    , musicVolume = musicVolume
    , soundVolume = soundVolume
    , visible = False
    }


view : String -> Model -> Html Msg
view extraMessage model =
    Html.div [ Attr.classList [ ( "settings", True ), ( "visible", model.visible ) ] ]
        [ Html.div []
            [ Html.h1 [] [ Html.text "Settings" ]
            , Html.p [] [ Html.text extraMessage ]
            ]
        , Html.div [ Attr.class "settingsList" ]
            [ Html.div []
                [ viewRangeSetting
                    { label = "Mouse sensitivity"
                    , value = model.mouseSensitivity
                    , onChange = MouseSensitivityUpdated
                    , min = 0.1
                    , max = 0.5
                    , step = 0.01
                    }
                , viewRangeSetting
                    { label = "Music volume"
                    , value = model.musicVolume
                    , onChange = MusicVolumeUpdated
                    , min = 0
                    , max = 1
                    , step = 0.05
                    }
                , viewRangeSetting
                    { label = "Sound volume"
                    , value = model.soundVolume
                    , onChange = SoundVolumeUpdated
                    , min = 0
                    , max = 1
                    , step = 0.05
                    }
                , Html.button [ Html.Events.onClick CloseSettings ]
                    [ Html.text "Close"
                    ]
                ]
            ]
        ]


viewRangeSetting : { label : String, min : Float, max : Float, step : Float, value : Float, onChange : Float -> msg } -> Html msg
viewRangeSetting { label, min, max, step, value, onChange } =
    Html.div [ Attr.class "settingRow" ]
        [ Html.div [] [ Html.text label ]
        , Html.div []
            [ Html.input
                [ Attr.type_ "range"
                , Attr.value (String.fromFloat value)
                , Attr.min (String.fromFloat min)
                , Attr.max (String.fromFloat max)
                , Attr.step (String.fromFloat step)
                , Html.Events.on "input" (D.map onChange rangeInputValueChangeDecoder)
                , Html.Events.on "change" (D.map onChange rangeInputValueChangeDecoder)
                ]
                []
            ]
        ]


rangeInputValueChangeDecoder : D.Decoder Float
rangeInputValueChangeDecoder =
    D.at [ "target", "value" ] D.string
        |> D.andThen
            (\valueAsString ->
                case String.toFloat valueAsString of
                    Just floatValue ->
                        D.succeed floatValue

                    Nothing ->
                        D.fail "Invalid number"
            )


show : Model -> Model
show model =
    { model | visible = True }
