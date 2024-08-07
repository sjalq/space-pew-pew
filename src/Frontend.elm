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
import Structures exposing (insertMaybe)
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
        [ Time.every moment (\time -> GameMsg (FrameTick time))
        , subscribeToKeyPresses
        ]


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , gameState = initState
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


updateGame : GameMsg -> GameState -> ( GameState, Cmd FrontendMsg )
updateGame msg gameState =
    case msg of
        FrameTick time ->
            let
                newBodies =
                    gameState.bodies |> Dict.map (\_ body -> move gameState.space body)

                finalBodies =
                    performCollisions newBodies

                newGameState =
                    { gameState
                        | bodies = finalBodies
                        , timeElapsed = gameState.timeElapsed + moment
                    }
            in
            ( newGameState, Cmd.none )

        FireProjectile shipId ->
            ( gameState, Cmd.none )

        Rotate direction bodyId ->
            let
                ship =
                    gameState.bodies
                        |> Dict.get bodyId
                        |> Maybe.map (rotate direction)
            in
            ( { gameState
                | bodies =
                    gameState.bodies
                        |> insertMaybe ship
              }
            , Cmd.none
            )

        Propel bodyId ->
            let
                bodyToAccelerate =
                    gameState.bodies
                        |> Dict.get bodyId
                        |> Maybe.map ship_propel
            in
            ( { gameState
                | bodies =
                    gameState.bodies
                        |> insertMaybe bodyToAccelerate
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


gameMsgToFrontendMsg : GameMsg -> FrontendMsg
gameMsgToFrontendMsg msg =
    GameMsg msg


htmlGameMsg : Html GameMsg -> Html FrontendMsg
htmlGameMsg =
    Html.map gameMsgToFrontendMsg


initState : GameState
initState =
    { bodies =
        [ { id = 1
          , mass = 1000
          , position = { x = 400, y = 300 }
          , velocity = { x = 0, y = 0 }
          , radius = 20
          , bodyType = Planet { gravity = 9.8 }
          }
        , { id = 2
          , mass = 50
          , position = { x = 100, y = 100 }
          , velocity = { x = 0, y = 0 }
          , radius = 100
          , bodyType =
                Ship
                    { rotation = 0
                    , propulsion = Newtonian { thrust = 1 }
                    , rotationSpeed = 0.1
                    }
          }
        , { id = 3
          , mass = 50
          , position = { x = 700, y = 500 }
          , velocity = { x = 0, y = 0 }
          , radius = 100
          , bodyType =
                Ship
                    { rotation = 0
                    , propulsion = Arilou { momentVelocity = 1 }
                    , rotationSpeed = 0.1
                    }
          }

        -- , { id = 4
        --   , mass = 10
        --   , position = { x = 500, y = 500 }
        --   , velocity = { x = 0, y = 0 }
        --   , radius = 5
        --   , bodyType = AltProjectile { damage = 5, lifetime = 10 }
        --   }
        ]
            |> List.map (\b -> ( b.id, b ))
            |> Dict.fromList
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


keyPressed : String -> GameMsg
keyPressed key =
    case key of
        "w" ->
            Propel 2

        "a" ->
            Rotate Left 2

        "d" ->
            Rotate Right 2

        "ArrowUp" ->
            Propel 3

        "ArrowLeft" ->
            Rotate Left 3

        "ArrowRight" ->
            Rotate Right 3

        _ ->
            let
                _ =
                    Debug.log "keyPressed" key
            in
            NoAction


altPerformCollisions : List Body -> List Body
altPerformCollisions bodies =
    let
        ( collidingBodies, nonCollidingBodies ) =
            bodies
                |> Physics.combinations
                |> List.map
                    (\combo ->
                        case combo of
                            [ a, b ] ->
                                Physics.collide a b

                            _ ->
                                ( False, [] )
                    )
                |> List.partition Tuple.first

        nonCollidingBodies_ =
            List.map Tuple.second nonCollidingBodies
                |> List.concat
                |> List.map (\b -> ( b.id, b ))
                |> Dict.fromList

        collidingBodies_ =
            List.map Tuple.second collidingBodies
                |> List.concat
                |> List.map (\b -> ( b.id, b ))
                |> Dict.fromList

        result =
            Dict.union collidingBodies_ nonCollidingBodies_
                |> Dict.values
    in
    result
