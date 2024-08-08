module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.Table
import Time
import Url


type alias Vector2D =
    { x : Float
    , y : Float
    }


type PropulsionType
    = Newtonian
        { thrust : Float
        }
    | LittleGrayMenTech
        { movementIncrement : Float
        }


type ProjectileType
    = Kenetic
        { damage : Int
        , lifetime : Float
        , initialSpeed : Float
        , hit : Bool
        }
    | Photonic
        { damage : Int
        , range : Float
        }


type BodyType
    = Conceptual
    | Planet
        { gravity : Float
        }
    | Ship
        { rotation : Float
        , rotationSpeed : Float
        , propulsion : PropulsionType
        , projectile : ProjectileType
        , crew : Int
        }
    | Projectile ProjectileType


type alias Body =
    { id : Int
    , mass : Float
    , position : Vector2D
    , velocity : Vector2D
    , radius : Float
    , bodyType : BodyType
    }


type alias Space =
    { width : Float
    , height : Float
    }


type alias GameState =
    { bodies : Evergreen.V1.Table.Table Body
    , timeElapsed : Float
    , space : Space
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , gameState : GameState
    }


type alias BackendModel =
    {}


type Direction
    = Left
    | Right


type GameMsg
    = NoAction
    | FrameTick Time.Posix
    | FireProjectile Int
    | Rotate Direction Int
    | Propel Int


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | GameMsg GameMsg
    | NewGame


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
