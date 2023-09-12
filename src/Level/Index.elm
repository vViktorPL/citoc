module Level.Index exposing (firstLevel, restLevels)

import Level exposing (Level)
import Level.Level_00
import Level.Level_01


firstLevel : Level
firstLevel = Level.Level_00.data

restLevels : List Level
restLevels = [Level.Level_01.data]