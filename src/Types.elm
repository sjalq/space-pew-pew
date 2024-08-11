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
    , lastPing : Time.Posix
    , lastPong : Time.Posix
    , pingTime : Int
    , emaPingTime : Float
    , depressedKeys : Set String
    }


type alias BackendModel =
    { gameCount : Int
    , pewsPewed : Int
    , trollbox : List ChatMessage
    , gameStates : Table GameState
    , clientCurrentGames : Dict ClientId GameId
    , lastSeen : Dict ClientId Time.Posix
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
    | Input InputMsg
    | UpdateGame Time.Posix
    | NewGame
    | SendChat
    | ChatInputChanged String
    | Ping Time.Posix
    | PongWithTime Time.Posix


type ToBackend
    = NoOpToBackend
    | StartNewGame
    | PewPewed
    | AddChat String
    | SubmitInput InputMsg
    | PingBackend Time.Posix
    | SubmitGameMsgs (List GameMsg)


type BackendMsg
    = NoOpBackendMsg
    | BEGameMsg GameId GameMsg
    | AddChatWithTime SessionId String Time.Posix
    | Tick Time.Posix
    | UpdateClients Time.Posix
    | Disconnect ClientId SessionId 
    | ClearOldClients Time.Posix


type ToFrontend
    = NoOpToFrontend
    | UpdateGlobal { gameCount : Int, pewsPewed : Int, trollbox : List ChatMessage }
    | UpdateGameState GameState
    | Pong



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
    | FrameTick (Set String) Time.Posix
      -- These are actions that have happened
    | FireProjectile Int
    | Rotate Direction Int
    | Propel Int


type InputMsg
    = KeyPressed String
    | KeyReleased String


fps : Float
fps =
    24


moment : Float
moment =
    1000 / fps
