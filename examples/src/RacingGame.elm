module RacingGame exposing (main)

import Compurob.Game3d.Racing.V1 as Racing


settings =
    { jeep =
        { texture = "jeep.png"
        , mesh = "jeep.obj"
        , settings = Nothing
        }
    }


main =
    Racing.game settings Racing.exampleWorld
