module Evergreen.V5.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V5.Table
import Set
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
        , maxCrew : Int
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
    { bodies : Evergreen.V5.Table.Table Body
    , timeElapsed : Float
    , space : Space
    , entropyCount : Int
    , depressedKeys : Set.Set String
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , gameState : GameState
    , gameCount : Int
    , pewsPewed : Int
    }


type alias BackendModel =
    { gameCount : Int
    , pewsPewed : Int
    }


type Direction
    = Left
    | Right


type GameMsg
    = NoAction
    | FrameTick Time.Posix
    | FireProjectile Int
    | Rotate Direction Int
    | Propel Int
    | KeyPressed String
    | KeyReleased String


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | GameMsg GameMsg
    | NewGame


type ToBackend
    = NoOpToBackend
    | NewGameStarted
    | PewPewed


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | GlobalUpdate
        { gameCount : Int
        , pewsPewed : Int
        }
