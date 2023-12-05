module Trigger exposing (Trigger, TriggerCondition(..), TriggerEffect(..), dependencies, isGlobalTrigger, localTrigger, narrowedToSector, tilesUsed)

import Assets exposing (Dependency)
import Color exposing (Color)
import Coordinates exposing (SectorCoordinates)
import Ending
import Length exposing (Length)
import LevelTile
import Orientation exposing (Orientation)


type alias Trigger =
    { conditions : List TriggerCondition
    , effects : List TriggerEffect
    }


localTrigger : SectorCoordinates -> List TriggerCondition -> List TriggerEffect -> Trigger
localTrigger sector conditions effects =
    Trigger (InSector sector :: conditions) effects


narrowedToSector : Trigger -> Maybe SectorCoordinates
narrowedToSector { conditions } =
    conditions
        |> List.filterMap
            (\cond ->
                case cond of
                    InSector sector ->
                        Just sector

                    _ ->
                        Nothing
            )
        |> List.head


isGlobalTrigger : Trigger -> Bool
isGlobalTrigger { conditions } =
    not
        (List.any
            (\cond ->
                case cond of
                    InSector _ ->
                        True

                    _ ->
                        False
            )
            conditions
        )


tilesUsed : Trigger -> List LevelTile.Model
tilesUsed trigger =
    trigger.effects
        |> List.concatMap
            (\tileEffect ->
                case tileEffect of
                    CreateTrigger subtrigger ->
                        tilesUsed subtrigger

                    ChangeTile _ tile ->
                        [ tile ]

                    _ ->
                        []
            )


dependencies : Trigger -> List Dependency
dependencies { effects } =
    List.concatMap
        (\effect ->
            case effect of
                CreateTrigger subtrigger ->
                    dependencies subtrigger

                PlaySound fileName ->
                    [ Assets.SoundEffectDep fileName ]

                PlayMusic fileName ->
                    [ Assets.MusicDep fileName ]

                StartNarration narrationIndex ->
                    [ Assets.SoundEffectDep ("narration_" ++ String.fromInt narrationIndex ++ ".mp3") ]

                ShowGameEndingScreen ->
                    Ending.dependencies

                OpenTerms _ ->
                    [ Assets.SoundEffectDep "elevator_door.mp3" ]

                --BreakWall _ -> [Assets.SoundEffectDep "rumble.mp3"]
                _ ->
                    []
        )
        effects


type TriggerCondition
    = LevelLoaded
    | InSector SectorCoordinates
    | EnteredFrom Orientation
    | LookAngle Orientation
    | LookingAtGround
    | NegativeHeadshake
    | Nod
    | SteppedIn
    | InSafeTeleportingOffset
    | CameBackToFloor
    | CounterEquals String Int
    | WindowShake



--| SectorCollision SectorCoordinates


type TriggerEffect
    = Teleport SectorCoordinates
    | SafeTeleport SectorCoordinates
    | NextLevel
    | ChangeTile SectorCoordinates LevelTile.Model
    | CreateTrigger Trigger
    | RemoveAllTriggersInSector ( Int, Int )
    | RemoveAllTriggersInSectors (List SectorCoordinates)
    | IncrementCounter String
    | DecrementCounter String
    | PlaySound String
    | InitFog Color Length
    | SitDown
    | OpenTerms SectorCoordinates
    | PlayMusic String
    | StartNarration Int
    | ShowGameEndingScreen
    | BreakWall SectorCoordinates
    | EnableUpsideDownWalking
    | ComeBackDown
