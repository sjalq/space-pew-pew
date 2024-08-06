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


normalizeV : Vector2D -> Vector2D
normalizeV vec =
    let
        magnitude =
            sqrt (vec.x ^ 2 + vec.y ^ 2)
    in
    if magnitude > 0 then
        scaleV (1 / magnitude) vec

    else
        vec


dotProduct : Vector2D -> Vector2D -> Float
dotProduct v1 v2 =
    v1.x * v2.x + v1.y * v2.y


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


altApplyForce : Vector2D -> AltBody -> AltBody
altApplyForce force body =
    let
        acceleration =
            scaleV (1 / body.mass) force
    in
    { body | velocity = addV body.velocity acceleration }


gravitationalForce : Body a -> Body a -> Vector2D
gravitationalForce bodyA bodyB =
    let
        distance =
            sqrt ((bodyA.position.x - bodyB.position.x) ^ 2 + (bodyB.position.y - bodyA.position.y) ^ 2)

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


altRocket_thrust : AltBody -> AltBody
altRocket_thrust body =
    case body.bodyType of
        AltShip ship ->
            body |> altApplyForce (angleToV ship.thrust ship.rotation)

        _ ->
            body


checkCollision : Body a -> Body a -> Bool
checkCollision bodyA bodyB =
    let
        distance =
            sqrt ((bodyA.position.x - bodyB.position.x) ^ 2 + (bodyA.position.y - bodyB.position.y) ^ 2)
    in
    distance <= bodyA.radius + bodyB.radius


altCheckCollision : AltBody -> AltBody -> Bool
altCheckCollision bodyA bodyB =
    let
        distance =
            sqrt ((bodyA.position.x - bodyB.position.x) ^ 2 + (bodyA.position.y - bodyB.position.y) ^ 2)
    in
    distance <= bodyA.radius + bodyB.radius


collide : AltBody -> AltBody -> ( AltBody, AltBody )
collide bodyA bodyB =
    if (bodyA.id /= bodyB.id) && altCheckCollision bodyA bodyB then
        let
            _ =
                Debug.log "collide" ( bodyA.id, bodyB.id )

            normal =
                Vector2D
                    (bodyB.position.x - bodyA.position.x)
                    (bodyB.position.y - bodyA.position.y)
                    |> normalizeV

            relativeVelocity =
                Vector2D
                    (bodyB.velocity.x - bodyA.velocity.x)
                    (bodyB.velocity.y - bodyA.velocity.y)

            velocityAlongNormal =
                dotProduct relativeVelocity normal

            -- Coefficient of restitution (elasticity)
            e =
                0.8

            -- Impulse scalar
            j =
                -(1 + e) * velocityAlongNormal / (1 / bodyA.mass + 1 / bodyB.mass)

            impulse =
                scaleV j normal

            newVelocityA =
                addV bodyA.velocity (scaleV (-1 / bodyA.mass) impulse)

            newVelocityB =
                addV bodyB.velocity (scaleV (1 / bodyB.mass) impulse)
        in
        ( { bodyA | velocity = newVelocityA }
        , { bodyB | velocity = newVelocityB }
        )

    else
        ( bodyA, bodyB )


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


altRotate : Direction -> AltBody -> AltBody
altRotate direction body =
    let
        newBodyType =
            case body.bodyType of
                AltShip ship ->
                    case direction of
                        Left ->
                            AltShip { ship | rotation = ship.rotation - ship.rotationSpeed }

                        Right ->
                            AltShip { ship | rotation = ship.rotation + ship.rotationSpeed }

                _ ->
                    body.bodyType
    in
    { body | bodyType = newBodyType }


isShip : AltBody -> Bool
isShip body =
    case body.bodyType of
        AltShip _ ->
            True

        _ ->
            False


isPlanet : AltBody -> Bool
isPlanet body =
    case body.bodyType of
        AltPlanet _ ->
            True

        _ ->
            False


isProjectile : AltBody -> Bool
isProjectile body =
    case body.bodyType of
        AltProjectile _ ->
            True

        _ ->
            False
