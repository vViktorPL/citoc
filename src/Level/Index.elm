module Level.Index exposing (firstLevel, restLevels)

import Level exposing (Level)
import Level.Level_01
import Level.Level_02
import Level.Level_04
import Level.Level_07
import Level.Level_08
import Level.Level_12
import Level.Level_13


firstLevel : Level
firstLevel = Level.Level_01.data

restLevels : List Level
restLevels = [Level.Level_02.data, Level.Level_04.data, Level.Level_07.data, Level.Level_08.data, Level.Level_12.data, Level.Level_13.data]