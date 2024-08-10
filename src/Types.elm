module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Table exposing (Table)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , gameCount : Int
    , pewsPewed : Int
    , chatInput : String
    , trollbox : List ChatMessage
    , gameState : GameState
    }


type alias BackendModel =
    { gameCount : Int
    , pewsPewed : Int
    , trollbox : List ChatMessage
    , gameStates : Table GameState
    , clientCurrentGames : Dict ClientId GameId
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


type ToBackend
    = NoOpToBackend
    | StartNewGame
    | PewPewed
    | AddChat String
    | SubmitCommand GameMsg


type BackendMsg
    = NoOpBackendMsg 
    | BEGameMsg GameId GameMsg
    | AddChatWithTime SessionId String Time.Posix


type ToFrontend
    = NoOpToFrontend
    | UpdateGlobal { gameCount : Int, pewsPewed : Int, trollbox : List ChatMessage }
    | UpdateGameState GameState



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


type alias GameId =
    Int


type alias GameState =
    { id : GameId
    , bodies : Table Body
    , timeElapsed : Float
    , space : Space
    , entropyCount : Int
    , depressedKeys : Set String
    , player1Id : ClientId
    , player2Id : ClientId
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
    1000 / 24
