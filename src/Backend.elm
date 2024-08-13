module Backend exposing (..)

import DebugApp
import Dict
import GameLoop
import L
import Lamdera
import Set
import Table
import Time
import Types exposing (..)


type alias Model =
    BackendModel



-- app =
--     Lamdera.backend
--         { init = init
--         , update = update
--         , updateFromFrontend = updateFromFrontend
--         , subscriptions = subscriptions
--         }


app =
    DebugApp.backend
        NoOpBackendMsg
        "34aa073d1876eed8"
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Time.every moment Tick
        , Time.every moment UpdateClients
        , Time.every 10000 ClearOldClients
        ]



--Sub.none
--Sub.none


init : ( Model, Cmd BackendMsg )
init =
    ( { gameStates = Table.empty
      , connectionCurrentGames = Dict.empty
      , lastSeen = Dict.empty
      , globalFun =
            { gameCount = 0
            , pewsPewed = 0
            , trollbox = []
            , totalLiveGames = 0
            }
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        AddChatWithTime browserId message time ->
            let
                chatMessage =
                    { timestamp = time
                    , browserId = browserId
                    , message = message
                    }

                oldGlobalFun =
                    model.globalFun

                newGlobalFun =
                    { oldGlobalFun | trollbox = oldGlobalFun.trollbox ++ [ chatMessage ] }

                newModel =
                    { model | globalFun = newGlobalFun }
            in
            ( newModel
            , L.broadcast (UpdateGlobal newGlobalFun)
            )

        Tick time ->
            let
                newGameStates =
                    model.gameStates
                        |> Table.map (\gameState -> GameLoop.updateMsg (FrameTick Set.empty time) gameState)
            in
            ( { model | gameStates = newGameStates }, Cmd.none )

        UpdateClients _ ->
            let
                gameCommands =
                    model.gameStates
                        |> Table.values
                        |> List.map
                            (\gameState ->
                                Cmd.batch
                                    [ L.sendToFrontend gameState.player1Id (UpdateGameState gameState)
                                    , L.sendToFrontend gameState.player2Id (UpdateGameState gameState)
                                    ]
                            )
            in
            ( model, Cmd.batch gameCommands )

        ClearOldClients time ->
            let
                -- if time - lastSeen > 60000 then
                --     remove clientId from lastSeen
                --     remove gameId from clientCurrentGames
                currentTime =
                    Time.posixToMillis time

                newLastSeen =
                    model.lastSeen
                        |> Dict.filter
                            (\_ lastSeenTime ->
                                let
                                    timeDiff =
                                        currentTime - Time.posixToMillis lastSeenTime
                                in
                                timeDiff <= 60000
                            )

                newConnectionCurrentGames =
                    model.connectionCurrentGames
                        |> Dict.filter
                            (\clientId _ ->
                                newLastSeen |> Dict.member clientId
                            )

                newGameStates =
                    model.gameStates
                        |> Table.filter
                            (\gameState ->
                                Dict.member gameState.player1Id newLastSeen && Dict.member gameState.player2Id newLastSeen
                            )

                newModel =
                    { model
                        | lastSeen = newLastSeen
                        , connectionCurrentGames = newConnectionCurrentGames
                        , gameStates = newGameStates
                    }
            in
            ( newModel, Cmd.none )

        HackPingBackend connectionId time ->
            let
                newModel =
                    { model | lastSeen = model.lastSeen |> Dict.insert connectionId time }
            in
            ( newModel, Cmd.none)


updateFromFrontend : BrowserId -> ConnectionId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend browserId connectionId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        StartNewGame ->
            let
                oldGlobalFun =
                    model.globalFun

                newGlobalFun =
                    { oldGlobalFun | gameCount = oldGlobalFun.gameCount + 1 }

                initState =
                    GameLoop.initState newGlobalFun.gameCount connectionId connectionId

                ( newId, newGameStates ) =
                    model.gameStates
                        |> Table.filter
                            (\gameState -> not (gameState.player1Id == connectionId) || not (gameState.player2Id == connectionId))
                        |> Table.insertReturningId initState

                newConnectionCurrentGames =
                    model.connectionCurrentGames
                        |> Dict.remove connectionId
                        |> Dict.insert connectionId newId

                newModel =
                    { model
                        | globalFun = newGlobalFun
                        , connectionCurrentGames = newConnectionCurrentGames
                        , gameStates = newGameStates
                    }
            in
            ( newModel
            , Cmd.batch
                [ L.broadcast (UpdateGlobal newGlobalFun)
                , L.sendToFrontend connectionId (UpdateGameState initState)
                , L.performWithTime (HackPingBackend connectionId)
                ]
            )

        PewPewed ->
            let
                oldGlobalFun =
                    model.globalFun

                newGlobalFun =
                    { oldGlobalFun | pewsPewed = oldGlobalFun.pewsPewed + 1 }

                newModel =
                    { model | globalFun = newGlobalFun }
            in
            ( newModel
            , L.broadcast (UpdateGlobal newGlobalFun)
            )

        AddChat message ->
            ( model, L.performWithTime (AddChatWithTime browserId message) )

        SubmitGameMsgs gameMsgs ->
            let
                newSpecificGameState =
                    model.connectionCurrentGames
                        |> Dict.get connectionId
                        |> Maybe.andThen
                            (\gameId ->
                                model.gameStates
                                    |> Table.get gameId
                                    |> Maybe.map (GameLoop.updateMsgs gameMsgs)
                            )

                _ =
                    Debug.log "newSpecificGameState__" (newSpecificGameState |> Maybe.map .id)

                newGameStates =
                    model.gameStates
                        |> Table.insertMaybe newSpecificGameState

                newModel =
                    { model | gameStates = newGameStates }
            in
            ( newModel, Cmd.none )

        PingBackend time ->
            let
                newModel =
                    { model | lastSeen = model.lastSeen |> Dict.insert connectionId time }
            in
            ( newModel, L.sendToFrontend connectionId Pong )
