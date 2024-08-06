module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , gameState : AltGameState
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GameMsg AltGameMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend



-- alt physics types stating with body and defining more specific types


type alias AltBody =
    { id : Int
    , mass : Float
    , position : Vector2D
    , velocity : Vector2D
    , radius : Float
    , bodyType : BodyType
    }


type BodyType
    = AltPlanet { gravity : Float }
    | AltShip { rotation : Float, thrust : Float, rotationSpeed : Float }
    | AltProjectile { damage : Int, lifetime : Float }



-- Game Types for Space Pew Pew!


type alias Ship =
    { position : Vector2D
    , velocity : Vector2D
    , rotation : Float
    , crew : Int
    , energy : Int
    , shipType : ShipType
    , radius : Float
    , mass : Float
    , thrust : Float -- the force of the ship's engines
    , rotationSpeed : Float
    }


type ShipType
    = Triangle


type alias Projectile =
    { mass : Float
    , position : Vector2D
    , velocity : Vector2D
    , radius : Float
    , damage : Int
    , lifetime : Float
    }


type alias Planet =
    { position : Vector2D
    , radius : Float
    , gravity : Float
    , mass : Float
    }


type alias Body a =
    { a
        | mass : Float
        , position : Vector2D
        , velocity : Vector2D
        , radius : Float
    }



-- type BodyType
--     = Planet { gravity : Float }
--     | Rocket { rotation : Float, thrust : Float }
--     | Projectile { damage : Int, lifetime : Float }


type alias Rocket a =
    Body
        { a
            | rotation : Float
            , thrust : Float
            , rotationSpeed : Float
        }


type alias Vector2D =
    { x : Float
    , y : Float
    }


type alias AltGameState =
    { bodies : List AltBody
    , timeElapsed : Float
    , space : Space
    }


type alias GameState =
    { ships : List Ship
    , projectiles : List Projectile
    , planet : Planet
    , timeElapsed : Float
    , space : Space
    }


type alias Space =
    { width : Float
    , height : Float
    }


type Direction
    = Left
    | Right


type alias Force =
    Float



-- Game Loop


type AltGameMsg
    = NoAction
    | FrameTick Time.Posix
    | FireProjectile
    | Rotate Direction
    | Accelerate


moment : Float
moment =
    1000 / 24
