module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , gameState : GameState
    }


type alias BackendModel =
    {
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GameMsg GameMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend



-- Game Types for Space Pew Pew!


type alias Ship =
    { position : Vector2D
    , velocity : Vector2D
    , rotation : Float
    , crew : Int
    , energy : Int
    , shipType : ShipType
    }


type ShipType
    = Triangle


type alias Projectile =
    { position : Vector2D
    , velocity : Vector2D
    , damage : Int
    , lifetime : Float
    }


type alias Planet =
    { position : Vector2D
    , radius : Float
    , gravity : Float
    }


type alias Vector2D =
    { x : Float
    , y : Float
    }


type alias GameState =
    { ships : List Ship
    , projectiles : List Projectile
    , planet : Planet
    , timeElapsed : Float
    }



-- Game Loop


type GameMsg
    = FrameTick Time.Posix
    | FireProjectile Ship
    | MoveShip Ship Vector2D


moment : Float
moment = 1000 / 24