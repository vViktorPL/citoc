module Orientation exposing (Orientation(..), decoder, encode)

import Json.Decode as D
import Json.Encode as E


type Orientation
    = North
    | East
    | South
    | West


decoder : D.Decoder Orientation
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "N" ->
                        D.succeed North

                    "E" ->
                        D.succeed East

                    "S" ->
                        D.succeed South

                    "W" ->
                        D.succeed West

                    _ ->
                        D.fail "Invalid orientation"
            )


encode : Orientation -> E.Value
encode orientation =
    case orientation of
        North ->
            E.string "N"

        East ->
            E.string "E"

        South ->
            E.string "S"

        West ->
            E.string "W"
