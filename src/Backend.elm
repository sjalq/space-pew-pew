module Backend exposing (..)

import L
import Lamdera exposing (ClientId, SessionId)
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    -- Sub.batch
    --     [ Time.every moment (\time -> BEGameMsg (FrameTick time))
    --     ]
    Sub.none


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

        BEGameMsg msg_ ->
            ( model, Cmd.none )


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
            ( model, L.performWithTime (AddChatWithTime sessionId message) )
