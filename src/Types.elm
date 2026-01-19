module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Table exposing (Table)
import Time
import Url exposing (Url)
import Set exposing (Set)
import Time
import Lamdera exposing (SessionId)


type alias FrontendModel =
    { key : Key
    , gameState : GameState
    , gameCount : Int
    , pewsPewed : Int
    , chatInput : String
    , trollbox : List ChatMessage
    , viewMode : ViewMode
    , modelViewer : ModelViewerState
    }

type ViewMode
    = GameView
    | ModelViewerView

type alias ModelViewerState =
    { selectedObject : ModelObject
    , rotationX : Float
    , rotationY : Float
    , rotationZ : Float
    , zoom : Float
    , autoRotate : Bool
    , wireframe : Bool
    , colorR : Float
    , colorG : Float
    , colorB : Float
    , useCustomColor : Bool
    }

type ModelObject
    = ModelHumanShip
    | ModelSaucer
    | ModelPlanet
    | ModelProjectile


type alias BackendModel =
    { gameCount : Int
    , pewsPewed : Int
    , trollbox : List ChatMessage
    }

type alias ChatMessage = 
    { timestamp : Time.Posix
    , clientId : SessionId
    , message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | FEGameMsg GameMsg
    | NewGame
    | SendChat 
    | ChatInputChanged String
    | ToggleViewMode
    | SelectModel ModelObject
    | UpdateRotationX Float
    | UpdateRotationY Float
    | UpdateRotationZ Float
    | UpdateZoom Float
    | ToggleAutoRotate
    | ToggleWireframe
    | ResetModelViewer
    | UpdateColorR Float
    | UpdateColorG Float
    | UpdateColorB Float
    | ToggleCustomColor


type ToBackend
    = NoOpToBackend
    | NewGameStarted
    | PewPewed
    | AddChat String


type BackendMsg
    = NoOpBackendMsg
    | BEGameMsg GameMsg
    | AddChatWithTime SessionId String Time.Posix


type ToFrontend
    = NoOpToFrontend
    | GlobalUpdate { gameCount : Int, pewsPewed : Int, trollbox : List ChatMessage }



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
