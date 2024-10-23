module Compurob.Game3d.Racing.V2 exposing
    ( game, World, exampleWorld
    , box, floor, slope
    , Settings
    , block, move, rotateX, rotateY, rotateZ, withPhysics
    )

{-| Adapted from <https://github.com/w0rm/elm-physics/blob/main/examples/src/RaycastCar.elm>
by Andrey Kuzmin.

  - `Game3d/Simulation/Car` — raycast vehicle simulation algorithm,
  - `Game3d/Model/Jeep` — load the 3d model using elm-obj-file.

Press arrow keys to drive, "b" to brake.

Try to add more obstacles to the demo by changing the initialWorld,
or tweak the settings in the `RayCastCar.Jeep` module!

@docs game, World, exampleWorld

@docs box, floor, slope

@docs Settings

@docs block, move, rotateX, rotateY, rotateZ, withPhysics

-}

import Acceleration
import Angle
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Compurob.Game3d.Model.Jeep as Jeep exposing (Jeep)
import Compurob.Game3d.Simulation.Car as Car exposing (Wheel)
import Direction3d
import Duration
import Eigenwijs.Playground as P
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Length exposing (Meters)
import Mass exposing (Mass)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World
import Pixels exposing (Pixels)
import Plane3d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Material
import Set exposing (Set)
import Task
import Vector3d
import Viewpoint3d


{-| Representing the 3D world you can add things to like slopes and obstacles.
-}
type alias World =
    World.World Id


{-| To be able to add entities generated from outside this library this type
holds identifying data for them.
-}
type alias EntityData =
    { name : String }


{-| To be able to add entities generated from outside this library we can
use this function to set a name as entity id when building bodies (Body).
-}
id name =
    Entity (EntityData name)


type Id
    = Obstacle Color (Block3d Meters BodyCoordinates)
    | Floor Color
    | Car (List (Wheel Id))
    | Entity EntityData


type alias Model state =
    { dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , world : World
    , jeep : Maybe Jeep
    , firstTick : Bool
    , speeding : Float
    , steering : Float
    , braking : Bool
    , settings : Maybe Car.Settings
    , state : state
    , playground : P.Picture
    , camera : Camera
    }


type Msg msg
    = Tick Float
    | Resize Int Int
    | KeyDown Command
    | KeyUp Command
    | JeepLoaded (Result String Jeep)
    | OverlayMsg msg


type Command
    = Speed Float
    | Steer Float
    | Brake
    | UseEyesOf Camera


type Camera
    = Fixed
    | Chauffeur
    | Spectator
    | Chaser


type alias Url =
    String


type alias State =
    { deltatime : Float
    , player : { position : ( Float, Float, Float ) }
    , controls : { keys : Set String }
    }


type alias Scene =
    List (Body Id)


{-| Settings include the url for the Jeep texture, its mesh file, and a
configurator function that takes the Jeep's settings so you can change
them. We use a function for configuring the car's properties because at
initialisation the Jeep is not loaded yet and we need some mesaurements
on it to correctly configure it.
-}
type alias Settings =
    { jeep : { texture : Url, mesh : Url, configurator : Car.Settings -> Car.Settings }
    }


type alias Update state =
    State -> state -> state


type alias Overlay state =
    state -> List (P.Shape P.PictureMsg)


{-| Create a racing game providing car settings, including texture and 3d mesh
file url.
-}
game : Update state -> Scene -> Overlay state -> state -> Settings -> Program () (Model state) (Msg P.PictureMsg)
game update scene overlay state settings =
    let
        world =
            scene
                |> List.foldl
                    World.add
                    (World.empty
                        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
                        |> World.add (Body.plane (Floor Color.white))
                    )
    in
    Browser.element
        { init = init settings world state
        , update = \msg model -> ( gameUpdate settings update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view overlay
        }


init : Settings -> World -> state -> () -> ( Model state, Cmd (Msg P.PictureMsg) )
init settings world state _ =
    ( { dimensions = ( Pixels.int 0, Pixels.int 0 )
      , world = world
      , jeep = Nothing
      , firstTick = True
      , speeding = 0
      , steering = 0
      , braking = False
      , settings = Nothing
      , state = state
      , playground = Tuple.first <| P.pictureInit ()
      , camera = Fixed
      }
    , Cmd.batch
        [ Task.perform
            (\{ viewport } ->
                Resize (round viewport.width) (round viewport.height)
            )
            Browser.Dom.getViewport
        , Jeep.load { texture = settings.jeep.texture, mesh = settings.jeep.mesh } |> Task.attempt JeepLoaded
        ]
    )


subscriptions : Model state -> Sub (Msg m)
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]


gameUpdate : Settings -> Update state -> Msg P.PictureMsg -> Model state -> Model state
gameUpdate settings update msg model =
    case msg of
        OverlayMsg m ->
            { model
                | playground = Tuple.first <| P.pictureUpdate m model.playground
            }

        JeepLoaded result ->
            case result of
                Ok jeep ->
                    { model
                        | jeep = Just jeep
                        , world =
                            World.add
                                (Body.compound jeep.collider (Car (Jeep.wheels jeep))
                                    |> Body.withBehavior (Body.dynamic (Mass.kilograms 4000))
                                    |> Body.moveTo (Point3d.meters 0 0 6)
                                )
                                model.world
                        , settings =
                            Just (settings.jeep.configurator (Jeep.settings jeep))
                    }

                _ ->
                    model

        Tick deltatime ->
            let
                pos =
                    model.world
                        |> World.bodies
                        |> List.foldl
                            (\body acc ->
                                case Body.data body of
                                    Car _ ->
                                        Body.frame body
                                            |> Frame3d.originPoint
                                            |> Point3d.unwrap

                                    _ ->
                                        acc
                            )
                            { x = 0, y = 0, z = 0 }
            in
            { model
                | state =
                    update
                        { deltatime = deltatime / 1000
                        , player = { position = ( pos.x, pos.y, pos.z ) }
                        , controls = { keys = Set.empty }
                        }
                        model.state
                , world =
                    case model.jeep of
                        Just jeep ->
                            model.world
                                |> World.update
                                    (\body ->
                                        case Body.data body of
                                            Car wheels ->
                                                let
                                                    ( newBody, newWheels ) =
                                                        Car.simulate
                                                            (Maybe.withDefault (Jeep.settings jeep) model.settings)
                                                            (Duration.seconds (1 / 60))
                                                            { worldWithoutCar =
                                                                World.keepIf
                                                                    (\b ->
                                                                        case Body.data b of
                                                                            Car _ ->
                                                                                False

                                                                            _ ->
                                                                                True
                                                                    )
                                                                    model.world
                                                            , speeding = model.speeding
                                                            , steering = model.steering
                                                            , braking = model.braking
                                                            }
                                                            wheels
                                                            body
                                                in
                                                Body.withData (Car newWheels) newBody

                                            _ ->
                                                body
                                    )
                                |> World.simulate (Duration.seconds (1 / 60))

                        _ ->
                            model.world
                                |> World.simulate (Duration.seconds (1 / 60))
                , firstTick = False
            }

        Resize width height ->
            { model | dimensions = ( Pixels.pixels width, Pixels.pixels height ) }

        KeyDown (Steer k) ->
            { model | steering = k }

        KeyDown (Speed k) ->
            { model | speeding = k }

        KeyUp (Steer k) ->
            { model
                | steering =
                    if k == model.steering then
                        0

                    else
                        model.steering
            }

        KeyUp (Speed k) ->
            { model
                | speeding =
                    if k == model.speeding then
                        0

                    else
                        model.speeding
            }

        KeyDown Brake ->
            { model | braking = True }

        KeyUp Brake ->
            { model | braking = False }

        KeyDown (UseEyesOf camera) ->
            { model | camera = camera }

        KeyUp (UseEyesOf _) ->
            model


view : Overlay state -> Model state -> Html (Msg P.PictureMsg)
view overlay { world, jeep, dimensions, settings, state, playground, camera } =
    Html.div []
        [ Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "z-index" "0"
            ]
            [ Scene3d.sunny
                { upDirection = Direction3d.z
                , sunlightDirection = Direction3d.xyZ (Angle.degrees -15) (Angle.degrees -45)
                , shadows = True
                , camera =
                    case camera of
                        Fixed ->
                            cameraFixed

                        Chauffeur ->
                            cameraLookingFrom (carFrameIn world) (Point3d.meters 0 -0.3 2.25)

                        Spectator ->
                            cameraLookingAt (carPositionIn world)

                        Chaser ->
                            cameraLookingFrom (carFrameIn world) (Point3d.meters 0 -5 4)
                , dimensions = dimensions
                , background = Scene3d.transparentBackground
                , clipDepth = Length.meters 0.1
                , entities =
                    List.map (bodyToEntity settings jeep) (World.bodies world)
                }
            ]
        , Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "z-index" "10"
            ]
            [ Html.map OverlayMsg (P.pictureView playground (overlay state))
            ]
        ]


cameraFixed : Camera3d Meters WorldCoordinates
cameraFixed =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters -40 40 30
                , focalPoint = Point3d.meters 0 -7 0
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 24
        }


carPositionIn world =
    List.foldl
        (\body pos ->
            case Body.data body of
                Car _ ->
                    Body.originPoint body

                _ ->
                    pos
        )
        (Point3d.meters 0 -10 0)
        (World.bodies world)


carFrameIn world =
    List.foldl
        (\body pos ->
            case Body.data body of
                Car _ ->
                    Body.frame body

                _ ->
                    pos
        )
        Frame3d.atOrigin
        (World.bodies world)


cameraLookingAt : Point3d Meters WorldCoordinates -> Camera3d Meters WorldCoordinates
cameraLookingAt target =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint =
                    Point3d.meters 0 -60 10
                        |> Point3d.translateBy
                            (Vector3d.from Point3d.origin target)
                , focalPoint =
                    Point3d.meters 0 0 -5
                        |> Point3d.translateBy
                            (Vector3d.from Point3d.origin target)
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        }


cameraLookingFrom : Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Point3d Meters BodyCoordinates -> Camera3d Meters WorldCoordinates
cameraLookingFrom frame point =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint =
                    point
                        |> Point3d.placeIn frame
                , focalPoint =
                    Point3d.meters 0 100 2
                        |> Point3d.placeIn frame
                , upDirection =
                    Frame3d.zDirection frame
                }
        , verticalFieldOfView = Angle.degrees 90
        }


bodyToEntity : Maybe Car.Settings -> Maybe Jeep -> Body Id -> Entity WorldCoordinates
bodyToEntity maybeSettings maybeJeep body =
    let
        frame =
            Body.frame body
    in
    Scene3d.placeIn frame <|
        case Body.data body of
            Floor color ->
                Scene3d.quad (Scene3d.Material.matte color)
                    (Point3d.meters -100 -100 0)
                    (Point3d.meters -100 100 0)
                    (Point3d.meters 100 100 0)
                    (Point3d.meters 100 -100 0)

            Obstacle color block_ ->
                Scene3d.blockWithShadow
                    (Scene3d.Material.nonmetal
                        { baseColor = color
                        , roughness = 1
                        }
                    )
                    block_

            Car wheels ->
                case maybeJeep of
                    Just jeep ->
                        Scene3d.group
                            (List.foldl
                                (\wheel entities ->
                                    let
                                        position =
                                            wheel.chassisConnectionPoint

                                        { downDirection, rightDirection } =
                                            Maybe.withDefault (Jeep.settings jeep) maybeSettings

                                        axisDirection =
                                            Axis3d.direction wheel.axis

                                        angle =
                                            if Quantity.greaterThan Quantity.zero (Direction3d.angleFrom rightDirection axisDirection) then
                                                identity

                                            else
                                                Frame3d.mirrorAcross Plane3d.yz

                                        newPosition =
                                            position |> Point3d.translateBy (Vector3d.withLength wheel.suspensionLength downDirection)

                                        newFrame =
                                            Frame3d.atOrigin
                                                |> angle
                                                |> Frame3d.rotateAround (Axis3d.through Point3d.origin (Direction3d.reverse rightDirection)) wheel.rotation
                                                |> Frame3d.rotateAround (Axis3d.through Point3d.origin downDirection) wheel.steering
                                                |> Frame3d.moveTo newPosition
                                    in
                                    (Scene3d.meshWithShadow jeep.material
                                        jeep.wheel
                                        jeep.wheelShadow
                                        |> Scene3d.placeIn newFrame
                                    )
                                        :: entities
                                )
                                [ Scene3d.meshWithShadow jeep.material
                                    jeep.chassis
                                    jeep.chassisShadow
                                ]
                                wheels
                            )

                    Nothing ->
                        Scene3d.nothing

            Entity a ->
                Scene3d.nothing


{-| A world with gravity as on Earth, a floor and some obstacles.
-}
exampleWorld : World
exampleWorld =
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
        |> World.add (Body.plane (Floor Color.white))
        |> World.add slope
        |> World.add (box (Point3d.meters 15 -15 0.5))
        |> World.add (box (Point3d.meters 15 -16.5 0.5))
        |> World.add (box (Point3d.meters 15 -18 0.5))
        |> World.add (box (Point3d.meters 15 -16 1.5))
        |> World.add (box (Point3d.meters 15 -17.5 1.5))
        |> World.add (box (Point3d.meters 15 -16.5 2.5))


{-| A floor to add to the world.
-}
floor : Color -> Body Id
floor color =
    Body.plane (Floor color)


{-| A slope to give a car the initial push.
-}
slope : Body Id
slope =
    let
        slopeBlock =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 16
                , Length.meters 0.5
                )
    in
    Body.block slopeBlock (Obstacle Color.white slopeBlock)
        |> Body.rotateAround Axis3d.x (Angle.radians (pi / 16))
        |> Body.moveTo (Point3d.meters 0 -2 1.5)


{-| Create a block Body which can be added to the world, with the supplied
dimensions (in meters), and a solid color.
-}
block : Color -> Float -> Float -> Float -> Body Id
block color w h d =
    let
        boxBlock =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters w
                , Length.meters h
                , Length.meters d
                )
    in
    Body.block boxBlock (Obstacle color boxBlock)


{-| Translate (move) a Body for the amounts of meters in the
x, y, and z direction.
-}
move : Float -> Float -> Float -> (Body data -> Body data)
move x y z =
    Body.moveTo (Point3d.meters x y z)


{-| Rotate a Body around the X axis.
-}
rotateX : Float -> Body data -> Body data
rotateX angle =
    Body.rotateAround Axis3d.x (Angle.degrees angle)


{-| Rotate a Body around the Y axis.
-}
rotateY : Float -> Body data -> Body data
rotateY angle =
    Body.rotateAround Axis3d.y (Angle.degrees angle)


{-| Rotate a Body around the Z axis.
-}
rotateZ : Float -> Body data -> Body data
rotateZ angle =
    Body.rotateAround Axis3d.z (Angle.degrees angle)


{-| Indicate that this Body is dynamic, it gets included in the physics
simulation, so gravity will be applied, which is why it asks for the mass
of the body.
-}
withPhysics : { mass : Mass } -> (Body data -> Body data)
withPhysics { mass } =
    Body.withBehavior (Body.dynamic mass)


{-| Create a box you can add to the world
-}
box : Point3d Meters WorldCoordinates -> Body Id
box position =
    let
        boxBlock =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 1
                , Length.meters 1
                , Length.meters 1
                )
    in
    Body.block boxBlock (Obstacle Color.lightGrey boxBlock)
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 10))
        |> Body.moveTo position


keyDecoder : (Command -> Msg m) -> Json.Decode.Decoder (Msg m)
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Json.Decode.succeed (toMsg (Steer -1))

                    "ArrowRight" ->
                        Json.Decode.succeed (toMsg (Steer 1))

                    "ArrowUp" ->
                        Json.Decode.succeed (toMsg (Speed 1))

                    "ArrowDown" ->
                        Json.Decode.succeed (toMsg (Speed -1))

                    "b" ->
                        Json.Decode.succeed (toMsg Brake)

                    "0" ->
                        Json.Decode.succeed (toMsg (UseEyesOf Fixed))

                    "1" ->
                        Json.Decode.succeed (toMsg (UseEyesOf Chauffeur))

                    "2" ->
                        Json.Decode.succeed (toMsg (UseEyesOf Spectator))

                    "3" ->
                        Json.Decode.succeed (toMsg (UseEyesOf Chaser))

                    _ ->
                        Json.Decode.fail ("Unrecognized key: " ++ string)
            )
