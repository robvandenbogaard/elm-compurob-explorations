module Compurob.Game3d.Model.Jeep exposing (Jeep, load, settings, wheels)

{-| elm-obj-file is used to decode various objects from the jeep model.

Body of the car, used for elm-physics simulation:

  - `convex-base_Base`
  - `convex-window_Window`

Positions of the wheels:

  - `front-right_Axis3d`
  - `rear-right_Axis3d`
  - `front-left_Axis3d`
  - `rear-left_Axis3d`

Used for rendering:

  - `chassis_Chassis`
  - `chassis-fender-left_fender-left`
  - `chassis-fender-right_fender-right`
  - `chassis-bumper_Bumper`
  - `chassis-cannister_Cannister`
  - `chassis-lamp-left_lamp-left`
  - `chassis-lamp-right_lamp-right`
  - `chassis-spare_Wheel`

Wheel, positioned at the origin:

  - `Wheel`

The Jeep model is courtesy of Kolja Wilcke <https://twitter.com/01k>

Adapted from <https://github.com/w0rm/elm-physics/blob/main/examples/src/RaycastCar/Jeep.elm>
by Andrey Kuzmin

@docs Jeep, load, settings, wheels

-}

import Array
import Axis3d exposing (Axis3d)
import BoundingBox3d
import Compurob.Game3d.Simulation.Car exposing (Settings, Wheel, defaultWheel)
import Direction3d
import Force
import Frame3d
import Http
import Length exposing (Length, Meters)
import Obj.Decode exposing (Decoder)
import Physics.Coordinates exposing (BodyCoordinates)
import Physics.Shape as Shape exposing (Shape)
import Point3d
import Polyline3d
import Quantity
import Scene3d.Material
import Scene3d.Mesh exposing (Shadow, Textured)
import Task exposing (Task)
import TriangularMesh


{-| The Jeep model contains all ingredients for being able to render a Jeep.
-}
type alias Jeep =
    { collider : List Shape
    , chassis : Textured BodyCoordinates
    , chassisShadow : Shadow BodyCoordinates
    , wheel : Textured BodyCoordinates
    , wheelRadius : Length
    , wheelWidth : Length
    , wheelShadow : Shadow BodyCoordinates
    , wheelAxes : List (Axis3d Meters BodyCoordinates)
    , material : Scene3d.Material.Textured BodyCoordinates
    }


{-| Load a 3D Jeep model from an obj file url, and a texture image file url.
-}
load : { texture : String, mesh : String } -> Task String Jeep
load urls =
    Http.task
        { method = "get"
        , headers = []
        , body = Http.emptyBody
        , url = urls.mesh
        , resolver =
            Http.stringResolver
                (\resp ->
                    case resp of
                        Http.GoodStatus_ _ str ->
                            Obj.Decode.decodeString (\a -> Length.meters (a / 2)) jeepDecoder str

                        _ ->
                            Err "Failed to load mesh"
                )
        , timeout = Nothing
        }
        |> Task.andThen
            (\fn ->
                Scene3d.Material.load urls.texture
                    |> Task.mapError (\_ -> "Failed to load texture")
                    |> Task.map (\texture -> fn (Scene3d.Material.texturedMatte texture))
            )


{-| Jeep settings define the physical properties of the Jeep, e.g. its engine
force, braking force, wheel radius etc.
-}
settings : Jeep -> Settings
settings jeep =
    { downDirection = Direction3d.negativeZ
    , rightDirection = Direction3d.negativeX
    , forwardDirection = Direction3d.negativeY
    , suspensionRestLength = Quantity.multiplyBy 0.4 jeep.wheelRadius
    , minSuspensionLength = Length.meters 0
    , maxSuspensionLength = Quantity.multiplyBy 1.2 jeep.wheelRadius
    , radius = jeep.wheelRadius
    , suspensionStiffness = 30
    , dampingCompression = 4.4
    , dampingRelaxation = 2.3
    , frictionSlip = 5
    , rollInfluence = 0.01
    , maxSuspensionForce = Force.newtons 100000
    , engineForce = Force.newtons 5000
    , brakingForce = Force.newtons 10000
    }


{-| Return a list of 4 wheels, configured for the Jeep.
-}
wheels : Jeep -> List (Wheel id)
wheels jeep =
    List.map
        (\axis ->
            { defaultWheel
                | chassisConnectionPoint =
                    Axis3d.originPoint axis
                        |> Point3d.translateIn Direction3d.z (Quantity.multiplyBy 0.2 jeep.wheelRadius)
                , axis = axis
            }
        )
        jeep.wheelAxes


jeepDecoder : Decoder (Scene3d.Material.Textured BodyCoordinates -> Jeep)
jeepDecoder =
    Obj.Decode.map5
        (\convexBase convexWindow chassis wheel wheelAxes ->
            let
                wheelBounds =
                    BoundingBox3d.hull Point3d.origin (TriangularMesh.vertices wheel |> Array.toList |> List.map .position)

                ( wheelWidth, wheelDiameter, _ ) =
                    BoundingBox3d.dimensions wheelBounds

                wheelMesh =
                    Scene3d.Mesh.texturedFaces wheel

                chassisMesh =
                    Scene3d.Mesh.texturedFaces chassis
            in
            \material ->
                { collider = [ Shape.unsafeConvex convexBase, Shape.unsafeConvex convexWindow ]
                , chassis = chassisMesh
                , chassisShadow = Scene3d.Mesh.shadow chassisMesh
                , wheel = wheelMesh
                , wheelShadow = Scene3d.Mesh.shadow wheelMesh
                , wheelAxes = wheelAxes
                , wheelRadius = Quantity.half wheelDiameter
                , wheelWidth = wheelWidth
                , material = material
                }
        )
        (Obj.Decode.object "convex-base_Base" (Obj.Decode.trianglesIn Frame3d.atOrigin))
        (Obj.Decode.object "convex-window_Window" (Obj.Decode.trianglesIn Frame3d.atOrigin))
        (startsWith "chassis" (Obj.Decode.texturedFacesIn Frame3d.atOrigin))
        (Obj.Decode.object "Wheel" (Obj.Decode.texturedFacesIn Frame3d.atOrigin))
        (Obj.Decode.combine
            [ Obj.Decode.object "front-left_Axis3d" axis3d
            , Obj.Decode.object "front-right_Axis3d" axis3d
            , Obj.Decode.object "rear-left_Axis3d" axis3d
            , Obj.Decode.object "rear-right_Axis3d" axis3d
            ]
        )


startsWith : String -> Decoder a -> Decoder a
startsWith prefix =
    Obj.Decode.filter
        (\properties ->
            case properties.object of
                Nothing ->
                    False

                Just object ->
                    String.startsWith prefix object
        )


axis3d : Decoder (Axis3d Meters BodyCoordinates)
axis3d =
    Obj.Decode.polylinesIn Frame3d.atOrigin
        |> Obj.Decode.andThen
            (\lines ->
                case lines of
                    line :: _ ->
                        case Polyline3d.vertices line of
                            p1 :: p2 :: _ ->
                                case Axis3d.throughPoints p1 p2 of
                                    Just axis ->
                                        Obj.Decode.succeed axis

                                    Nothing ->
                                        Obj.Decode.fail "Failed to constuct axis"

                            _ ->
                                Obj.Decode.fail "Expected at least two points"

                    _ ->
                        Obj.Decode.fail "Expected at least one line"
            )
