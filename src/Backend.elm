module Backend exposing (..)

import Dict
import GameLoop
import L
import Lamdera exposing (ClientId, SessionId)
import Table
import Time
import Types exposing (..)
import Set


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
    Sub.batch
        [ Time.every moment Tick
        , Time.every moment UpdateClients
        , Lamdera.onDisconnect Disconnect
        ]



--Sub.none
--Sub.none


init : ( Model, Cmd BackendMsg )
init =
    ( { gameCount = 0
      , pewsPewed = 0
      , trollbox = []
      , gameStates = Table.empty
      , clientCurrentGames = Dict.empty
      }
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
                (UpdateGlobal
                    { gameCount = newModel.gameCount
                    , pewsPewed = newModel.pewsPewed
                    , trollbox = newModel.trollbox
                    }
                )
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

        UpdateClients time ->
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

        Disconnect clientId sessionId ->
            let
                newModel =
                    { model | gameStates = Table.filter (\gameState -> gameState.player1Id /= clientId && gameState.player2Id /= clientId) model.gameStates }
            in
            ( newModel, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        StartNewGame ->
            let
                newGameCount =
                    model.gameCount + 1

                initState =
                    GameLoop.initState newGameCount clientId clientId

                ( newId, newGameStates ) =
                    model.gameStates |> Table.insertReturningID initState

                newModel =
                    { model
                        | gameCount = newGameCount
                        , clientCurrentGames = model.clientCurrentGames |> Dict.insert clientId newId
                        , gameStates = newGameStates
                    }
            in
            ( newModel
            , Cmd.batch
                [ L.broadcast
                    (UpdateGlobal
                        { gameCount = newModel.gameCount
                        , pewsPewed = newModel.pewsPewed
                        , trollbox = model.trollbox
                        }
                    )
                , L.sendToFrontend clientId (UpdateGameState initState)
                ]
            )

        PewPewed ->
            let
                newModel =
                    { model | pewsPewed = model.pewsPewed + 1 }
            in
            ( newModel
            , L.broadcast
                (UpdateGlobal
                    { gameCount = newModel.gameCount
                    , pewsPewed = newModel.pewsPewed
                    , trollbox = model.trollbox
                    }
                )
            )

        AddChat message ->
            ( model, L.performWithTime (AddChatWithTime sessionId message) )

        SubmitInput inputMsg ->
            -- let
            --     newSpecificGameState =
            --         model.clientCurrentGames
            --             |> Dict.get clientId
            --             |> Maybe.andThen
            --                 (\gameId ->
            --                     model.gameStates
            --                         |> Table.get gameId
            --                         |> Maybe.map (GameLoop.updateInputs gameMsg)
            --                 )

            --     newGameStates =
            --         Table.insertMaybe newSpecificGameState model.gameStates

            --     newModel =
            --         { model | gameStates = newGameStates }
            -- in
            ( model, Cmd.none )

        SumbitGameMsgs gameMsgs ->
            let
                newSpecificGameState =
                    model.clientCurrentGames
                        |> Dict.get clientId
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

        PingBackend ->
            ( model, L.sendToFrontend clientId Pong )
