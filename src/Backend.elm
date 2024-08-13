module Backend exposing (..)

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


app =
    Lamdera.backend
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

        BEGameMsg gameId msg_ ->
            let
                newGameState =
                    Table.get gameId model.gameStates
                        |> Maybe.map (GameLoop.updateMsg msg_)
            in
            case newGameState of
                Just gameState ->
                    if gameState.id /= 0 then
                        ( { model
                            | gameStates = Table.insert gameState model.gameStates
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

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
                    Dict.filter
                        (\_ lastSeenTime ->
                            let
                                timeDiff =
                                    currentTime - Time.posixToMillis lastSeenTime
                            in
                            timeDiff <= 60000
                        )
                        model.lastSeen

                newConnectionCurrentGames =
                    Dict.filter
                        (\clientId _ ->
                            newLastSeen |> Dict.member clientId
                        )
                        model.connectionCurrentGames

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
                    model.gameStates |> Table.insertReturningID initState


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
                        |> Dict.get browserId
                        |> Maybe.andThen
                            (\gameId ->
                                model.gameStates
                                    |> Table.get gameId
                                    |> Maybe.map (GameLoop.updateMsgs gameMsgs)
                            )

                newGameStates =
                    Table.insertMaybe newSpecificGameState model.gameStates

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
