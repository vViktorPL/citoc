module Level.Index exposing (firstLevel, restLevels)

import Level exposing (Level)
import Level.Level_00
import Level.Level_01
import Level.Level_02
import Level.Level_10
import Level.Level_11


firstLevel : Level
firstLevel = Level.Level_00.data

restLevels : List Level
restLevels = [Level.Level_01.data, Level.Level_02.data, Level.Level_10.data, Level.Level_11.data]