module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)
import L

type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { gameCount = 0, pewsPewed = 0 }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        NewGameStarted ->
            let
                newModel = { model | gameCount = model.gameCount + 1 }
            in
            
            ( newModel, L.broadcast (GlobalUpdate { gameCount = newModel.gameCount, pewsPewed = newModel.pewsPewed }) )   

        PewPewed -> 
            let
                newModel = { model | pewsPewed = model.pewsPewed + 1 }
            in
            ( newModel, L.broadcast (GlobalUpdate { gameCount = newModel.gameCount, pewsPewed = newModel.pewsPewed }) )
    