module GameLoop exposing (..)

import L exposing (..)
import Lamdera exposing (ClientId)
import Physics exposing (..)
import Set exposing (Set)
import Table exposing (Table)
import Types exposing (..)


initState : Int -> ClientId -> ClientId -> GameState
initState gameCount player1 player2 =
    { id = 0
    , player1Id = player1
    , player2Id = player2
    , bodies =
        [ { id = 1
          , mass = 100000000000
          , position = { x = 600, y = 350 }
          , velocity = { x = 0, y = 0 }
          , radius = 50
          , bodyType = Planet { gravity = 9.8 }
          }
        , { id = 2
          , mass = 50
          , position = { x = 100, y = 100 }
          , velocity = { x = 0, y = 0 }
          , radius = 50
          , bodyType =
                Ship
                    { rotation = 0
                    , propulsion = Newtonian { thrust = 1 }
                    , rotationSpeed = 0.1
                    , projectile = Kenetic { damage = 1, lifetime = 1000, initialSpeed = 1, hit = False }
                    , maxCrew = 30
                    , crew = 30
                    }
          }
        , { id = 3
          , mass = 50
          , position = { x = 700, y = 500 }
          , velocity = { x = 0, y = 0 }
          , radius = 32
          , bodyType =
                Ship
                    { rotation = 0
                    , propulsion = LittleGrayMenTech { movementIncrement = 20 }
                    , rotationSpeed = tau / 32
                    , projectile = Kenetic { damage = 1, lifetime = 1000, initialSpeed = 1, hit = False }
                    , maxCrew = 10
                    , crew = 10
                    }
          }
        ]
            |> Table.fromList
    , timeElapsed = 0
    , space = { width = 1200, height = 700 }
    , entropyCount = gameCount
    , depressedKeys = Set.empty
    }


updateGame : GameMsg -> GameState -> ( GameState, Cmd BackendMsg )
updateGame msg gameState =
    case msg of
        NoAction ->
            ( gameState, Cmd.none )

        FrameTick time ->
            let
                newBodies =
                    gameState.bodies
                        |> applyGravityToAll
                        |> updateLifetimes
                        |> Table.map (\body -> applyVelocity gameState.space body)

                finalBodies =
                    performCollisions newBodies
                        |> Table.filter (not << projectile_destroyed)
                        |> Table.filter (not << ship_destroyed)

                newGameState =
                    { gameState
                        | bodies = finalBodies
                        , timeElapsed = gameState.timeElapsed + moment
                    }

                performKeys =
                    gameState.depressedKeys
                        |> Set.toList
                        |> List.map keyToMsg
                        |> List.map (BEGameMsg gameState.id)
                        |> List.map performNow
                        |> Cmd.batch
            in
            ( newGameState
            , performKeys
            )

        FireProjectile shipId ->
            let
                -- find the ship
                -- generate the appropriate projectile
                ship =
                    gameState.bodies
                        |> Table.get shipId

                newProjectile =
                    ship
                        |> Maybe.andThen ship_fireProjectile

                newBodies =
                    Table.insertMaybe newProjectile gameState.bodies

                newState =
                    { gameState | bodies = newBodies }
            in
            ( newState, L.sendToBackend PewPewed )

        Rotate direction bodyId ->
            let
                ship =
                    gameState.bodies
                        |> Table.get bodyId
                        |> Maybe.map (rotate direction)
            in
            ( { gameState
                | bodies =
                    gameState.bodies
                        |> Table.insertMaybe ship
              }
            , Cmd.none
            )

        Propel bodyId ->
            let
                bodyToAccelerate =
                    gameState.bodies
                        |> Table.get bodyId
                        |> Maybe.map ship_propel
            in
            ( { gameState
                | bodies =
                    gameState.bodies
                        |> Table.insertMaybe bodyToAccelerate
              }
            , Cmd.none
            )

        KeyPressed key ->
            ( { gameState | depressedKeys = Set.insert (String.toLower key) gameState.depressedKeys }, Cmd.none )

        KeyReleased key ->
            ( { gameState | depressedKeys = Set.remove (String.toLower key) gameState.depressedKeys }, Cmd.none )


keyToMsg : String -> GameMsg
keyToMsg key =
    let
        key_ =
            String.toLower key
    in
    case key_ of
        "w" ->
            Propel 2

        "a" ->
            Rotate Left 2

        "d" ->
            Rotate Right 2

        "f" ->
            FireProjectile 2

        "g" ->
            FireProjectile 2

        "arrowup" ->
            Propel 3

        "arrowleft" ->
            Rotate Left 3

        "arrowright" ->
            Rotate Right 3

        "shift" ->
            FireProjectile 3

        "control" ->
            FireProjectile 3

        _ ->
            NoAction
