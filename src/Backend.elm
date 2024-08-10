module Backend exposing (..)

import Dict
import GameLoop
import L
import Lamdera exposing (ClientId, SessionId)
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
subscriptions model =
    []
        ++ (model.gameStates
                --|> Table.filter (\gameState -> gameState.id == "0")
                |> Table.values
                |> List.map (\gameState -> Time.every moment (\time -> BEGameMsg gameState.id (FrameTick time)))
           )
        |> Sub.batch



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
                        |> Maybe.map (GameLoop.updateGame msg_)
            in
            case newGameState of
                Just ( gameState, gameCmds ) ->
                    if gameState.id /= 0 then
                        ( { model
                            | gameStates = Table.insert gameState model.gameStates
                          }
                        , Cmd.batch
                            [ gameCmds
                            , L.sendToFrontend gameState.player1Id (UpdateGameState gameState)
                            , L.sendToFrontend gameState.player2Id (UpdateGameState gameState)
                            ]
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


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

        SubmitCommand gameMsg ->
            let
                _ =
                    Debug.log "games__" (model.clientCurrentGames |> Dict.size)

                gameCmds =
                    model.clientCurrentGames
                        |> Dict.get clientId
                        |> Maybe.map (\gameId -> BEGameMsg gameId gameMsg |> L.performNow)
                        |> Maybe.withDefault Cmd.none
            in
            ( model, gameCmds )
