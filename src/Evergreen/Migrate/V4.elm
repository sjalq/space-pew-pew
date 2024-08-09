module Evergreen.Migrate.V4 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Dict
import Evergreen.V3.Table
import Evergreen.V3.Types
import Evergreen.V4.Table
import Evergreen.V4.Types
import Lamdera.Migrations exposing (..)
import Set


frontendModel : Evergreen.V3.Types.FrontendModel -> ModelMigration Evergreen.V4.Types.FrontendModel Evergreen.V4.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V3.Types.BackendModel -> ModelMigration Evergreen.V4.Types.BackendModel Evergreen.V4.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V3.Types.FrontendMsg -> MsgMigration Evergreen.V4.Types.FrontendMsg Evergreen.V4.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V3.Types.ToBackend -> MsgMigration Evergreen.V4.Types.ToBackend Evergreen.V4.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V3.Types.BackendMsg -> MsgMigration Evergreen.V4.Types.BackendMsg Evergreen.V4.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V3.Types.ToFrontend -> MsgMigration Evergreen.V4.Types.ToFrontend Evergreen.V4.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_FrontendModel : Evergreen.V3.Types.FrontendModel -> Evergreen.V4.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , gameState = old.gameState |> migrate_Types_GameState
    , gameCount = old.gameCount
    , pewsPewed = old.pewsPewed
    }


migrate_Table_Table : (a_old -> a_new) -> Evergreen.V3.Table.Table a_old -> Evergreen.V4.Table.Table a_new
migrate_Table_Table migrate_a old =
    case old of
        Evergreen.V3.Table.Table p0 p1 ->
            Evergreen.V4.Table.Table p0 (p1 |> Dict.map (\k -> migrate_a))


migrate_Types_Body : Evergreen.V3.Types.Body -> Evergreen.V4.Types.Body
migrate_Types_Body old =
    { id = old.id
    , mass = old.mass
    , position = old.position |> migrate_Types_Vector2D
    , velocity = old.velocity |> migrate_Types_Vector2D
    , radius = old.radius
    , bodyType = old.bodyType |> migrate_Types_BodyType
    }


migrate_Types_BodyType : Evergreen.V3.Types.BodyType -> Evergreen.V4.Types.BodyType
migrate_Types_BodyType old =
    case old of
        Evergreen.V3.Types.Conceptual ->
            Evergreen.V4.Types.Conceptual

        Evergreen.V3.Types.Planet p0 ->
            Evergreen.V4.Types.Planet p0

        Evergreen.V3.Types.Ship p0 ->
            Evergreen.V4.Types.Ship
                { rotation = p0.rotation
                , rotationSpeed = p0.rotationSpeed
                , propulsion = p0.propulsion |> migrate_Types_PropulsionType
                , projectile = p0.projectile |> migrate_Types_ProjectileType
                , crew = p0.crew
                }

        Evergreen.V3.Types.Projectile p0 ->
            Evergreen.V4.Types.Projectile (p0 |> migrate_Types_ProjectileType)


migrate_Types_Direction : Evergreen.V3.Types.Direction -> Evergreen.V4.Types.Direction
migrate_Types_Direction old =
    case old of
        Evergreen.V3.Types.Left ->
            Evergreen.V4.Types.Left

        Evergreen.V3.Types.Right ->
            Evergreen.V4.Types.Right


migrate_Types_FrontendMsg : Evergreen.V3.Types.FrontendMsg -> Evergreen.V4.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V3.Types.UrlClicked p0 ->
            Evergreen.V4.Types.UrlClicked p0

        Evergreen.V3.Types.UrlChanged p0 ->
            Evergreen.V4.Types.UrlChanged p0

        Evergreen.V3.Types.NoOpFrontendMsg ->
            Evergreen.V4.Types.NoOpFrontendMsg

        Evergreen.V3.Types.GameMsg p0 ->
            Evergreen.V4.Types.GameMsg (p0 |> migrate_Types_GameMsg)

        Evergreen.V3.Types.NewGame ->
            Evergreen.V4.Types.NewGame


migrate_Types_GameMsg : Evergreen.V3.Types.GameMsg -> Evergreen.V4.Types.GameMsg
migrate_Types_GameMsg old =
    case old of
        Evergreen.V3.Types.NoAction ->
            Evergreen.V4.Types.NoAction

        Evergreen.V3.Types.FrameTick p0 ->
            Evergreen.V4.Types.FrameTick p0

        Evergreen.V3.Types.FireProjectile p0 ->
            Evergreen.V4.Types.FireProjectile p0

        Evergreen.V3.Types.Rotate p0 p1 ->
            Evergreen.V4.Types.Rotate (p0 |> migrate_Types_Direction) p1

        Evergreen.V3.Types.Propel p0 ->
            Evergreen.V4.Types.Propel p0


migrate_Types_GameState : Evergreen.V3.Types.GameState -> Evergreen.V4.Types.GameState
migrate_Types_GameState old =
    { bodies = old.bodies |> migrate_Table_Table migrate_Types_Body
    , timeElapsed = old.timeElapsed
    , space = old.space |> migrate_Types_Space
    , entropyCount = old.entropyCount
    , depressedKeys = Set.empty
    }


migrate_Types_ProjectileType : Evergreen.V3.Types.ProjectileType -> Evergreen.V4.Types.ProjectileType
migrate_Types_ProjectileType old =
    case old of
        Evergreen.V3.Types.Kenetic p0 ->
            Evergreen.V4.Types.Kenetic p0

        Evergreen.V3.Types.Photonic p0 ->
            Evergreen.V4.Types.Photonic p0


migrate_Types_PropulsionType : Evergreen.V3.Types.PropulsionType -> Evergreen.V4.Types.PropulsionType
migrate_Types_PropulsionType old =
    case old of
        Evergreen.V3.Types.Newtonian p0 ->
            Evergreen.V4.Types.Newtonian p0

        Evergreen.V3.Types.LittleGrayMenTech p0 ->
            Evergreen.V4.Types.LittleGrayMenTech p0


migrate_Types_Space : Evergreen.V3.Types.Space -> Evergreen.V4.Types.Space
migrate_Types_Space old =
    old


migrate_Types_Vector2D : Evergreen.V3.Types.Vector2D -> Evergreen.V4.Types.Vector2D
migrate_Types_Vector2D old =
    old