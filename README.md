# Compurob explorations

Experimental libraries for use in the Compurob workshop.

## Racing game

Adapted from <https://github.com/w0rm/elm-physics/blob/main/examples/src/RaycastCar>.

The `Compurob.Game3d.Racing.V1` module packs the ray cast car example of elm-physics
into an easy to use, and configurable form:

``` elm
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
    Racing.game settings
```

The Jeep's `settings` can be set to car simulation settings. They are defined in
`Compurob.Game3d.Simulation.Car.Settings`. The `texture` and `mesh` parameters
expect urls.
