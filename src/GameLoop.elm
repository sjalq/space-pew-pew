module GameLoop exposing (..)

import L exposing (..)
import Lamdera exposing (ClientId)
import Physics exposing (..)
import Set exposing (Set)
import Table
import Types exposing (..)


rotationAt60Fps =
    0.1


lgmAt60Fps =
    1


thrustAt60Fps =
    1


initState : Int -> ClientId -> ClientId -> GameState
initState gameCount player1 player2 =
    { id = -1
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
                    , rotationSpeed = 0.1 -- 0.1 is what feels nice at 60fps. so at 24 fps that would be...4
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
    , space = { width = 1200, height = 600 }
    , entropyCount = gameCount
    }


updateMsg : GameMsg -> GameState -> GameState
updateMsg msg gameState =
    case msg of
        NoAction ->
            gameState

        FrameTick depressedKeys _ ->
            let
                gameState_commandsApplied =
                    updateMsgs (keysToGameMsgs depressedKeys) gameState

                newBodies =
                    gameState_commandsApplied.bodies
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
            in
            newGameState

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
            newState

        Rotate direction bodyId ->
            let
                ship =
                    gameState.bodies
                        |> Table.get bodyId
                        |> Maybe.map (rotate direction)
            in
            { gameState
                | bodies =
                    gameState.bodies
                        |> Table.insertMaybe ship
            }

        Propel bodyId ->
            let
                bodyToAccelerate =
                    gameState.bodies
                        |> Table.get bodyId
                        |> Maybe.map ship_propel
            in
            { gameState
                | bodies =
                    gameState.bodies
                        |> Table.insertMaybe bodyToAccelerate
            }


updateMsgs : List GameMsg -> GameState -> GameState
updateMsgs msgs gameState =
    let
        _ =
            Debug.log "updateMsgs__" msgs

        _ =
            Debug.log "gameState__" gameState
    in
    List.foldl updateMsg gameState msgs


updateInputs : InputMsg -> Set String -> Set String
updateInputs msg_ depressedKeys =
    case msg_ of
        KeyPressed key ->
            Set.insert (String.toLower key) depressedKeys

        KeyReleased key ->
            Set.remove (String.toLower key) depressedKeys


keysToGameMsgs : Set String -> List GameMsg
keysToGameMsgs depressedKeys =
    depressedKeys
        |> Set.toList
        |> List.map keyToMsg


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
