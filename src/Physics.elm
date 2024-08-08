module Physics exposing (..)

import Table exposing (Table)
import Types exposing (..)



-- Vector operations


addV : Vector2D -> Vector2D -> Vector2D
addV v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }


subV : Vector2D -> Vector2D -> Vector2D
subV v1 v2 =
    { x = v1.x - v2.x
    , y = v1.y - v2.y
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


magnitudeV : Vector2D -> Float
magnitudeV vec =
    sqrt (vec.x ^ 2 + vec.y ^ 2)


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


gravitationalConstant : Float
gravitationalConstant =
    6.6743e-11


gravitationalForce : Body -> Body -> Vector2D
gravitationalForce bodyA bodyB =
    let
        distanceVector =
            subV bodyB.position bodyA.position

        distance_ =
            magnitudeV distanceVector
                |> Debug.log "distance__"

        forceMagnitude =
            if distance_ == 0 then
                0

            else
                (gravitationalConstant * bodyA.mass * bodyB.mass) / (distance_ ^ 2)
    in
    scaleV forceMagnitude (normalizeV distanceVector)


applyGravityToAll : Table Body -> Table Body
applyGravityToAll bodies =
    let
        forceOnBody body =
            bodies
                |> Table.remove body.id
                |> Table.toList
                |> List.map (gravitationalForce body)
                |> List.foldl addV { x = 0, y = 0 }

        newBody body =
            body |> applyForce (forceOnBody body) |> Debug.log "newBody__"
    in
    Table.map newBody bodies


updateLifetimes : Table Body -> Table Body
updateLifetimes bodies =
    bodies
        |> Table.filterMap updateLifetime


updateLifetime : Body -> Maybe Body
updateLifetime body =
    -- if a body is projectile, this is relevant
    -- if a projectile is Kenetic, this is relevant
    case body.bodyType of
        Projectile projectile ->
            case projectile of
                Kenetic rec ->
                    if rec.lifetime < 0 then
                        Nothing

                    else
                        Just
                            { body
                                | bodyType =
                                    Projectile (Kenetic { rec | lifetime = rec.lifetime - moment })
                            }

                _ ->
                    Just body

        _ ->
            Just body


ship_propel : Body -> Body
ship_propel body =
    case body.bodyType of
        Ship ship ->
            case ship.propulsion of
                Newtonian { thrust } ->
                    body |> applyForce (angleToV thrust ship.rotation)

                LittleGrayMenTech { movementIncrement } ->
                    { body
                        | position = addV body.position (scaleV movementIncrement (angleToV 1 ship.rotation))
                        , velocity = { x = 0, y = 0 }
                    }

        _ ->
            body


ship_fireProjectile : Body -> Maybe Body
ship_fireProjectile body =
    case body.bodyType of
        Ship ship ->
            let
                projectile =
                    case ship.projectile of
                        Kenetic rec ->
                            let
                                initialVelocity =
                                    angleToV rec.initialSpeed ship.rotation
                                        |> addV body.velocity
                            in
                            { id = 0
                            , radius = 2
                            , position = addV body.position (angleToV body.radius ship.rotation)
                            , velocity = addV body.velocity initialVelocity
                            , mass = 1
                            , bodyType =
                                Projectile
                                    (Kenetic
                                        { rec
                                            | damage = rec.damage
                                            , initialSpeed = rec.initialSpeed
                                            , lifetime = rec.lifetime
                                        }
                                    )
                            }

                        Photonic rec ->
                            { id = 0
                            , radius = 2
                            , position = body.position
                            , velocity = body.velocity
                            , mass = 0
                            , bodyType =
                                Projectile
                                    (Photonic
                                        { rec
                                            | damage = rec.damage
                                            , range = rec.range
                                        }
                                    )
                            }
            in
            Just projectile

        _ ->
            Nothing


checkCollision : Body -> Body -> Bool
checkCollision bodyA bodyB =
    distance bodyA.position bodyB.position <= bodyA.radius + bodyB.radius


distance : Vector2D -> Vector2D -> Float
distance v1 v2 =
    sqrt ((v1.x - v2.x) ^ 2 + (v1.y - v2.y) ^ 2)


collide : Body -> Body -> ( Bool, List Body )
collide bodyA bodyB =
    let
        distance_ =
            distance bodyA.position bodyB.position

        collision =
            (bodyA.id /= bodyB.id) && checkCollision bodyA bodyB
    in
    if collision then
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

            overlapDistance =
                (bodyA.radius + bodyB.radius) - distance_

            totalMass =
                bodyA.mass + bodyB.mass

            correctionVectorA =
                scaleV (overlapDistance * (bodyB.mass / totalMass)) normal

            correctionVectorB =
                scaleV (overlapDistance * (bodyA.mass / totalMass)) normal

            correctedPositionA =
                subV bodyA.position correctionVectorA

            correctedPositionB =
                addV bodyB.position correctionVectorB
        in
        ( True
        , [ { bodyA | position = correctedPositionA, velocity = newVelocityA }
          , { bodyB | position = correctedPositionB, velocity = newVelocityB }
          ]
        )

    else
        ( False, [ bodyA, bodyB ] )


interact : Body -> Body -> List Body
interact bodyA bodyB =
    case ( bodyA.bodyType, bodyB.bodyType ) of
        ( Projectile (Kenetic rec), Ship ship ) ->
            [ { bodyA | bodyType = Projectile (Kenetic { rec | hit = True }) }
            , { bodyB | bodyType = Ship { ship | crew = ship.crew - rec.damage } }
            ]

        ( Ship ship, Projectile (Kenetic rec) ) ->
            [ { bodyA | bodyType = Ship { ship | crew = ship.crew - rec.damage } }
            , { bodyB | bodyType = Projectile (Kenetic { rec | hit = True }) }
            ]

        _ ->
            [ bodyA, bodyB ]


performCollisions : Table Body -> Table Body
performCollisions bodies =
    let
        ( collidingBodies, nonCollidingBodies ) =
            bodies
                |> Table.values
                |> combinations
                |> List.map
                    (\combo ->
                        case combo of
                            [ a, b ] ->
                                let
                                    ( collided, newBodies ) =
                                        collide a b

                                    newBodies_ =
                                        case ( collided, newBodies ) of
                                            ( True, [ a_, b_ ] ) ->
                                                interact a_ b_

                                            _ ->
                                                newBodies
                                in
                                ( collided, newBodies_ )

                            _ ->
                                ( False, [] )
                    )
                |> List.partition Tuple.first

        nonCollidingBodies_ =
            List.map Tuple.second nonCollidingBodies
                |> List.concat
                |> Table.fromList

        collidingBodies_ =
            List.map Tuple.second collidingBodies
                |> List.concat
                |> Table.fromList

        result =
            Table.union collidingBodies_ nonCollidingBodies_
    in
    result


projectile_destroyed : Body -> Bool
projectile_destroyed body =
    case body.bodyType of
        Projectile (Kenetic rec) ->
            rec.hit

        _ ->
            False


ship_destroyed : Body -> Bool
ship_destroyed body =
    case body.bodyType of
        Ship ship ->
            ship.crew <= 0

        _ ->
            False


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
