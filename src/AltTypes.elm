module AltTypes exposing (..)

import Types exposing (Vector2D)


type alias BodyId =
    Int



-- Instances


type alias Body =
    { id : BodyId
    , mass : Float
    , position : Vector2D
    , velocity : Vector2D
    , radius : Float -- should be more the boundary than the actual radius
    }


type alias PlanetInstance =
    { id : BodyId
    , planetBP : Int
    }


type alias ShipInstance =
    { id : BodyId
    , shipBP : Int
    , crew : Int
    }


type alias BulletInstance =
    { id : BodyId
    , projectileBP : Int
    , remainingLifetime : Float
    }


type alias MineInstance =
    { id : BodyId
    , mineBP : Int
    }


type alias LaserInstance =
    { id : BodyId
    , laserBP : Int
    , startingPosition : Vector2D
    , endingPosition : Vector2D
    }



-- Blueprints
-- Planet Blueprints


type PlanetType
    = BlueAndGreen
    | Red
    | BrownGasGiant
    | GreenGasGiant
    | IceGiant


type alias PlanetBP =
    { id : Int
    , planetType : PlanetType
    , mass : Float
    , radius : Float
    }



-- Projectile Blueprints


type ProjectileType
    = Bullet
    | Mine


type alias BulletBP =
    { id : Int
    , damage : Float
    , lifetime : Float
    }


type alias MineBP =
    { id : Int
    , damage : Float
    }



-- Ship Blueprints


type ShipType
    = Human
    | LGM


type alias ShipBP =
    { id : Int
    , shipType : ShipType
    , propulsion : PropultionType
    , weapon : WeaponType
    , crew : Int
    }



-- Propulsion Blueprints => Part of Ship Blueprint


type PropultionType
    = Rocket
    | LGM_Propulsion


type alias RocketBP =
    { id : Int
    , thrust : Float
    }


type alias LGMBP =
    { id : Int
    , jumpDistance : Float
    }



-- Weapon Blueprints => Part of Ship Blueprint


type WeaponType
    = Bullets
    | Laser


type alias LaserBP =
    { id : Int
    , damage : Float
    , distance : Float
    }
