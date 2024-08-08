module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Table exposing (Table)
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
        }
    | Projectile
        { damage : Int
        , lifetime : Float
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


moment : Float
moment =
    1000 / 60
