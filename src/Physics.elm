module Physics exposing (..)

import Dict exposing (Dict)
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


degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * (pi / 180)


combinations : List a -> List (List a)
combinations list =
    case list of
        [] ->
            []

        x :: xs ->
            (xs |> List.map (\y -> [ x, y ])) ++ combinations xs



-- Physics


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


move : Space -> Body -> Body
move space body =
    let
        newPosition =
            body.position
                |> addV (scaleV moment body.velocity)
                |> wrapVectorToSpace space
    in
    { body | position = newPosition }


applyForce : Vector2D -> Body -> Body
applyForce force body =
    let
        acceleration =
            scaleV (1 / body.mass) force
    in
    { body | velocity = addV body.velocity acceleration }


gravitationalForce : Body -> Body -> Vector2D
gravitationalForce bodyA bodyB =
    let
        distance =
            sqrt ((bodyA.position.x - bodyB.position.x) ^ 2 + (bodyB.position.y - bodyA.position.y) ^ 2)

        angleBetween =
            atan2 (bodyB.position.y - bodyA.position.y) (bodyB.position.x - bodyA.position.x)
    in
    angleBetween |> angleToV (bodyA.mass * bodyB.mass / distance ^ 2)


centerOfMass : List Body -> Vector2D
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


ship_propel : Body -> Body
ship_propel body =
    case body.bodyType of
        Ship ship ->
            case ship.propulsion of
                Newtonian { thrust } ->
                    body |> applyForce (angleToV thrust ship.rotation)

                Arilou { momentVelocity } ->
                    { body
                        | position = addV body.position (scaleV momentVelocity (angleToV 1 ship.rotation))
                        , velocity = { x = 0, y = 0 }
                    }

        _ ->
            body


checkCollision : Body -> Body -> Bool
checkCollision bodyA bodyB =
    let
        distance =
            sqrt ((bodyA.position.x - bodyB.position.x) ^ 2 + (bodyA.position.y - bodyB.position.y) ^ 2)
    in
    distance <= bodyA.radius + bodyB.radius


collide : Body -> Body -> ( Bool, List Body )
collide bodyA bodyB =
    if (bodyA.id /= bodyB.id) && checkCollision bodyA bodyB then
        let
            _ =
                Debug.log "collision" ( bodyA.id, bodyB.id )

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
        ( True
        , [ { bodyA | velocity = newVelocityA }
          , { bodyB | velocity = newVelocityB }
          ]
        )

    else
        ( False, [ bodyA, bodyB ] )


performCollisions : Dict Int Body -> Dict Int Body
performCollisions bodies =
    let
        ( collidingBodies, nonCollidingBodies ) =
            bodies
                |> Dict.values
                |> combinations
                |> List.map
                    (\combo ->
                        case combo of
                            [ a, b ] ->
                                collide a b

                            _ ->
                                ( False, [] )
                    )
                |> List.partition Tuple.first

        nonCollidingBodies_ =
            List.map Tuple.second nonCollidingBodies
                |> List.concat
                |> List.map (\b -> ( b.id, b ))
                |> Dict.fromList

        collidingBodies_ =
            List.map Tuple.second collidingBodies
                |> List.concat
                |> List.map (\b -> ( b.id, b ))
                |> Dict.fromList

        result =
            Dict.union collidingBodies_ nonCollidingBodies_
    in
    result


rotate : Direction -> Body -> Body
rotate direction body =
    let
        newBodyType =
            case body.bodyType of
                Ship ship ->
                    case direction of
                        Left ->
                            Ship { ship | rotation = ship.rotation - ship.rotationSpeed }

                        Right ->
                            Ship { ship | rotation = ship.rotation + ship.rotationSpeed }

                _ ->
                    body.bodyType
    in
    { body | bodyType = newBodyType }
