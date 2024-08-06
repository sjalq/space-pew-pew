module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Lamdera
import List.Extra
import Physics exposing (..)
import RenderSvg exposing (renderGame)
import Svg exposing (..)
import Time
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 / 24) (\time -> GameMsg (FrameTick time))
        , subscribeToKeyPresses
        ]


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , gameState = initAltState
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GameMsg gameMsg ->
            let
                ( newGameState, cmd ) =
                    model.gameState |> updateGame gameMsg

                newModel =
                    { model | gameState = newGameState }
            in
            ( newModel, cmd )


updateGame : AltGameMsg -> AltGameState -> ( AltGameState, Cmd FrontendMsg )
updateGame msg gameState =
    case msg of
        FrameTick time ->
            let
                newBodies =
                    gameState.bodies |> List.map (move gameState.space)

                finalBodies =
                    altPerformCollisions
                        newBodies

                newGameState =
                    { gameState
                        | bodies = finalBodies
                        , timeElapsed = gameState.timeElapsed + moment
                    }
            in
            ( newGameState, Cmd.none )

        FireProjectile ->
            ( gameState, Cmd.none )

        Rotate direction ->
            let
                firstShip =
                    gameState.bodies
                        |> List.filter isShip
                        |> List.sortBy .id
                        |> List.head
            in
            ( { gameState
                | bodies =
                    gameState.bodies
                        |> List.map
                            (\body ->
                                if Just body.id == (firstShip |> Maybe.map .id) then
                                    altRotate direction body

                                else
                                    body
                            )
              }
              --( gameState
            , Cmd.none
            )

        Accelerate ->
            let
                firstShip =
                    gameState.bodies
                        |> List.filter isShip
                        |> List.sortBy .id
                        |> List.head
            in
            ( { gameState
                | bodies =
                    gameState.bodies
                        |> List.map
                            (\body ->
                                if Just body.id == (firstShip |> Maybe.map .id) then
                                    altRocket_thrust body

                                else
                                    body
                            )
              }
            , Cmd.none
            )

        NoAction ->
            ( gameState, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ renderGame model.gameState |> htmlGameMsg
                ]
            ]
        ]
    }


gameMsgToFrontendMsg : AltGameMsg -> FrontendMsg
gameMsgToFrontendMsg msg =
    GameMsg msg


htmlGameMsg : Html AltGameMsg -> Html FrontendMsg
htmlGameMsg =
    Html.map gameMsgToFrontendMsg


initState : GameState
initState =
    { ships =
        [ { position = { x = 100, y = 100 }
          , velocity = { x = 0.0, y = 0.0 }
          , rotation = 0
          , crew = 5
          , energy = 100
          , shipType = Triangle
          , radius = 100
          , mass = 100
          , thrust = 1
          , rotationSpeed = 0.5
          }
        , { position = { x = 700, y = 500 }
          , velocity = { x = 0, y = 0 }
          , rotation = 0
          , crew = 3
          , energy = 80
          , shipType = Triangle
          , radius = 100
          , mass = 100
          , thrust = 1
          , rotationSpeed = 0.1
          }
        ]
    , projectiles = [] -- No projectiles (pew pew) as requested
    , planet =
        { position = { x = 400, y = 300 }
        , radius = 50
        , gravity = 9.8
        , mass = 1000
        }
    , timeElapsed = 0
    , space = { width = 800, height = 600 }
    }


initAltState : AltGameState
initAltState =
    { bodies =
        [ { id = 1
          , mass = 100
          , position = { x = 400, y = 300 }
          , velocity = { x = 0, y = 0 }
          , radius = 20
          , bodyType = AltPlanet { gravity = 9.8 }
          }
        , { id = 2
          , mass = 50
          , position = { x = 100, y = 100 }
          , velocity = { x = 0, y = 0 }
          , radius = 100
          , bodyType = AltShip { rotation = 0, thrust = 1, rotationSpeed = 0.1 }
          }
        , { id = 3
          , mass = 50
          , position = { x = 700, y = 500 }
          , velocity = { x = 0, y = 0 }
          , radius = 100
          , bodyType = AltShip { rotation = 0, thrust = 1, rotationSpeed = 0.1 }
          }

        -- , { id = 4
        --   , mass = 10
        --   , position = { x = 500, y = 500 }
        --   , velocity = { x = 0, y = 0 }
        --   , radius = 5
        --   , bodyType = AltProjectile { damage = 5, lifetime = 10 }
        --   }
        ]
    , timeElapsed = 0
    , space = { width = 800, height = 600 }
    }


listOfBodies model =
    model.gameState.ships ++ model.gameState.projectiles


subscribeToKeyPresses : Sub FrontendMsg
subscribeToKeyPresses =
    Browser.Events.onKeyDown (Decode.map keyPressed keyDecoder)
        |> Sub.map GameMsg


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


keyPressed : String -> AltGameMsg
keyPressed key =
    case String.toLower key of
        "w" ->
            Accelerate

        "a" ->
            Rotate Left

        "d" ->
            Rotate Right

        _ ->
            NoAction



-- checkAllCollisions : List (Body a) -> List (Body a)
-- checkAllCollisions bodies =
--     bodies
--         |> List.concatMap
--             (\body ->
--                 bodies
--                     |> List.map (\other -> Physics.checkCollision body other)
--             )


altPerformCollisions : List AltBody -> List AltBody
altPerformCollisions bodies =
    bodies
        |> List.concatMap (\body -> bodies |> List.map (\other -> Physics.collide body other))
        |> List.concatMap (\( a, b ) -> [ a, b ])
        |> List.map (\body -> ( body.id, body ))
        |> Dict.fromList
        |> Dict.values
