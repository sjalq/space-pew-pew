module GameLoop exposing (..)

import Types exposing (..)
import Physics exposing (..)
import Table exposing (Table)
import Set exposing (Set)
import L exposing (..)

updateGame : GameMsg -> GameState -> ( GameState, Cmd FrontendMsg )
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
                        |> List.map FEGameMsg
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