module Backend exposing (..)

import L
import Lamdera exposing (ClientId, SessionId)
import Task
import Time
import Types exposing (..)


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
    ( { gameCount = 0, pewsPewed = 0, trollbox = [] }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        AddChatWithTime clientId message time ->
            let
                chatMessage =
                    { timestamp = time
                    , clientId = clientId
                    , message = message
                    }

                newModel =
                    { model | trollbox = model.trollbox ++ [ chatMessage ] }
            in
            ( newModel
            , L.broadcast
                (GlobalUpdate
                    { gameCount = newModel.gameCount
                    , pewsPewed = newModel.pewsPewed
                    , trollbox = newModel.trollbox
                    }
                )
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        NewGameStarted ->
            let
                newModel =
                    { model | gameCount = model.gameCount + 1 }
            in
            ( newModel
            , L.broadcast
                (GlobalUpdate
                    { gameCount = newModel.gameCount
                    , pewsPewed = newModel.pewsPewed
                    , trollbox = model.trollbox
                    }
                )
            )

        PewPewed ->
            let
                newModel =
                    { model | pewsPewed = model.pewsPewed + 1 }
            in
            ( newModel
            , L.broadcast
                (GlobalUpdate
                    { gameCount = newModel.gameCount
                    , pewsPewed = newModel.pewsPewed
                    , trollbox = model.trollbox
                    }
                )
            )

        AddChat message ->
            ( model, performWithTime (AddChatWithTime sessionId message) )


performWithTime : (Time.Posix -> a) -> Cmd a
performWithTime f =
    Time.now |> Task.perform f
