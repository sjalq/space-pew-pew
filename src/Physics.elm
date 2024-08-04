module Physics exposing (..)

import Types exposing (..)
import Html exposing (a)


move : Space -> Body a -> Body a
move space body =
    let
        newX =
            (body.position.x + body.velocity.x * moment) |> wrapCoordinate space.width

        newY =
            (body.position.y + body.velocity.y * moment) |> wrapCoordinate space.height
    in
    { body
        | position =
            { x = newX
            , y = newY
            }
    }

accelerationFromThrustToMass : Float -> Float -> Float
accelerationFromThrustToMass thrust_ mass =
    thrust_ / mass

thrust : Rocket a -> Rocket a
thrust rocket =
    let
        acceleration =
            accelerationFromThrustToMass rocket.thrust rocket.mass
    in
    accelerate acceleration rocket


accelerate : Float ->  Rocket a -> Rocket a
accelerate acceleration rocket =
    let
        oldVelocity =
            rocket.velocity

        rotation =
            rocket.rotation

        accelerationVector =
            { x = acceleration * cos rotation, y = acceleration * sin rotation }

        newVelocity =
            { oldVelocity | x = oldVelocity.x + accelerationVector.x, y = oldVelocity.y + accelerationVector.y }
    in
    { rocket
        | velocity = newVelocity
    }


checkCollision : Body a -> Body a -> Bool
checkCollision bodyA bodyB =
    let
        distance =
            sqrt ((bodyA.position.x - bodyB.position.x) ^ 2 + (bodyA.position.y - bodyB.position.y) ^ 2)
    in
    distance <= bodyA.radius + bodyB.radius


transferMomentum : Body a -> Body a -> ( Body a, Body a )
transferMomentum bodyA bodyB =
    let
        totalMass =
            bodyA.mass + bodyB.mass

        thing1Velocity =
            bodyA.velocity

        thing2Velocity =
            bodyB.velocity

        newVelocity1 =
            { bodyA
                | velocity =
                    { thing1Velocity
                        | x = thing1Velocity.x * (bodyA.mass / totalMass)
                        , y = thing1Velocity.y * (bodyA.mass / totalMass)
                    }
            }

        newVelocity2 =
            { bodyB
                | velocity =
                    { thing2Velocity
                        | x = thing2Velocity.x * (bodyB.mass / totalMass)
                        , y = thing2Velocity.y * (bodyB.mass / totalMass)
                    }
            }

        newThing1 =
            { bodyA | velocity = newVelocity1.velocity }

        newThing2 =
            { bodyB | velocity = newVelocity2.velocity }
    in
    ( newThing1, newThing2 )


wrapCoordinate : Float -> Float -> Float
wrapCoordinate max value =
    if value < 0 then
        max + (value |> round |> modBy (round max) |> toFloat)

    else if value > max then
        value |> round |> modBy (round max) |> toFloat

    else
        value

degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * (pi / 180)


rotate : Direction -> Rocket a -> Rocket a
rotate direction thing=
    let
        -- remember to rotate by radians == 5 degrees
        newRotation =
            case direction of
                Left ->
                    thing.rotation - (degreesToRadians 5)

                Right ->
                    thing.rotation + (degreesToRadians 5)
    in
    { thing | rotation = newRotation }
