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
    {}


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
    , radius : Float
    , mass : Float
    , thrust : Float -- the force of the ship's engines
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
    }


type alias Body a =
    { a
        | mass : Float
        , position : Vector2D
        , velocity : Vector2D
        , radius : Float
    }

type alias Rocket a =
    Body { a | rotation : Float, thrust : Float }


type alias Vector2D =
    { x : Float
    , y : Float
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


type alias Force = Float



-- Game Loop


type GameMsg
    = NoAction
    | FrameTick Time.Posix
    | FireProjectile 
    | Rotate Direction
    | Accelerate 


moment : Float
moment =
    1000 / 24
