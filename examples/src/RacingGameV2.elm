module RacingGameV2 exposing (main)

import Color exposing (..)
import Compurob.Game3d.Racing.V2 as Racing exposing (..)
import Eigenwijs.Playground as P
import Force
import Mass


jeepConfigurator settings =
    { settings
        | engineForce = Force.newtons 10000
        , brakingForce = Force.newtons 15000
    }


scene =
    [ block green 10 2 1 |> move 1 5 0
    , block red 10 20 0.25 |> rotateX 30
    , block grey 1 1 2 |> withPhysics { mass = Mass.kilograms 3 } |> move 0 0 10
    ]


type Phase
    = Driving Time { position : ( Float, Float, Float ) }
    | Finished Time


type alias Time =
    Float


main =
    Racing.game update
        scene
        overlay
        (Driving 0 { position = ( 0, 0, 0 ) })
        { jeep =
            { texture = "/workshop/uploads/Jeep.png"
            , mesh = "/workshop/uploads/Jeep.obj.txt"
            , configurator = jeepConfigurator
            }
        }


finish =
    ( 100, 0, 0 )


distanceBetween ( x1, y1, z1 ) ( x2, y2, z2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)


update { player, controls, deltatime } phase =
    case phase of
        Driving time _ ->
            if distanceBetween player.position finish < 1 then
                Finished time

            else
                Driving (time + deltatime) player

        _ ->
            phase


overlay phase =
    let
        roundedTime time =
            toFloat (round (time * 10)) / 10
    in
    case phase of
        Driving time player ->
            let
                ( x, y, z ) =
                    player.position
            in
            [ P.words P.black (String.fromFloat (roundedTime time)) |> P.scale 2
            , P.words P.black (String.fromInt (round x)) |> P.moveDown 20
            ]

        Finished time ->
            [ P.words P.black "Finished!" |> P.scale 3
            , P.words P.black "Your time:" |> P.moveDown 20
            , P.words P.black (String.fromFloat (roundedTime time)) |> P.scale 2 |> P.moveDown 40
            ]
