module Physics exposing (..)

import Types exposing (..)



-- Vector operations


addV : Vector2D -> Vector2D -> Vector2D
addV v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }


scaleV : Float -> Vector2D -> Vector2D
scaleV scalar vec =
    { x = scalar * vec.x
    , y = scalar * vec.y
    }


angleToV : Float -> Float -> Vector2D
angleToV magnitude angle =
    { x = magnitude * cos angle
    , y = magnitude * sin angle
    }


wrapVectorToSpace : Space -> Vector2D -> Vector2D
wrapVectorToSpace space vec =
    let
        wrapCoordinate max value =
            if value < 0 then
                max + (value |> round |> modBy (round max) |> toFloat)

            else if value > max then
                value |> round |> modBy (round max) |> toFloat

            else
                value
    in
    { x = wrapCoordinate space.width vec.x
    , y = wrapCoordinate space.height vec.y
    }



-- Physics


move : Space -> Body a -> Body a
move space body =
    let
        newPosition =
            body.position
                |> addV (scaleV moment body.velocity)
                |> wrapVectorToSpace space
    in
    { body | position = newPosition }


applyForce : Vector2D -> Body a -> Body a
applyForce force body =
    let
        acceleration =
            scaleV (1 / body.mass) force
    in
    { body | velocity = addV body.velocity acceleration }

gravitationalForce : Body a -> Body a -> Vector2D
gravitationalForce bodyA bodyB =
    let
        distance =
            sqrt ((bodyA.position.x - bodyB.position.x) ^ 2 + (bodyA.position.y - bodyB.position.y) ^ 2)

        angleBetween =
            atan2 (bodyB.position.y - bodyA.position.y) (bodyB.position.x - bodyA.position.x)
    in
    angleBetween |> angleToV (bodyA.mass * bodyB.mass / distance ^ 2) 


centerOfMass : List (Body a) -> Vector2D
centerOfMass bodies =
    let
        totalMass =
            List.foldl (\body acc -> acc + body.mass) 0 bodies

        weightedPositions =
            List.map (\body -> scaleV body.mass body.position) bodies

        sumPositions =
            List.foldl addV { x = 0, y = 0 } weightedPositions
    in
    if totalMass > 0 then
        scaleV (1 / totalMass) sumPositions
    else
        { x = 0, y = 0 }


rocket_thrust : Rocket a -> Rocket a
rocket_thrust rocket =
    rocket |> applyForce (angleToV rocket.thrust rocket.rotation)


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

        calculateNewVelocity body otherBody =
            let
                massRatio =
                    body.mass / totalMass

                otherMassRatio =
                    otherBody.mass / totalMass
            in
            addV
                (scaleV massRatio body.velocity)
                (scaleV otherMassRatio otherBody.velocity)

        newVelocityA =
            calculateNewVelocity bodyA bodyB

        newVelocityB =
            calculateNewVelocity bodyB bodyA

        newBodyA =
            { bodyA | velocity = newVelocityA }

        newBodyB =
            { bodyB | velocity = newVelocityB }
    in
    ( newBodyA, newBodyB )


degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * (pi / 180)


rotate : Direction -> Rocket a -> Rocket a
rotate direction rocket =
    let
        newRotation =
            case direction of
                Left ->
                    rocket.rotation - rocket.rotationSpeed

                Right ->
                    rocket.rotation + rocket.rotationSpeed
    in
    { rocket | rotation = newRotation }
