module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Table exposing (Table)
import Time
import Url exposing (Url)
import Set exposing (Set)


type alias FrontendModel =
    { key : Key
    , gameState : GameState
    , gameCount : Int
    , pewsPewed : Int
    }


type alias BackendModel =
    { gameCount : Int
    , pewsPewed : Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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
    | GlobalUpdate { gameCount : Int, pewsPewed : Int }



-- alt physics types stating with body and defining more specific types


type alias Body =
    { id : Int
    , mass : Float
    , position : Vector2D
    , velocity : Vector2D
    , radius : Float
    , bodyType : BodyType
    }


type BodyType
    = Conceptual
    | Planet { gravity : Float }
    | Ship
        { rotation : Float
        , rotationSpeed : Float
        , propulsion : PropulsionType
        , projectile : ProjectileType
        , maxCrew : Int
        , crew : Int
        }
    | Projectile ProjectileType


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


type PropulsionType
    = Newtonian { thrust : Float }
    | LittleGrayMenTech { movementIncrement : Float }


type alias Vector2D =
    { x : Float
    , y : Float
    }


type alias GameState =
    { bodies : Table Body
    , timeElapsed : Float
    , space : Space
    , entropyCount : Int
    , depressedKeys : Set String
    }


type alias Space =
    { width : Float
    , height : Float
    }


type Direction
    = Left
    | Right



-- Game Loop


type GameMsg
    = NoAction
    | FrameTick Time.Posix
    | FireProjectile Int
    | Rotate Direction Int
    | Propel Int
    | KeyPressed String
    | KeyReleased String


moment : Float
moment =
    1000 / 60
