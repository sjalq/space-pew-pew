module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Lamdera
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
            --update the position of the ships and based on their velocity
            let
                newShips =
                    gameState.ships |> List.map (move gameState.space)

                newProjectiles =
                    gameState.projectiles |> List.map (move gameState.space)

                newGameState =
                    { gameState
                        | ships = newShips
                        , projectiles = newProjectiles
                        , timeElapsed = gameState.timeElapsed + moment
                    }
            in
            ( newGameState, Cmd.none )

        FireProjectile ->
            ( gameState, Cmd.none )

        Rotate direction ->
            case gameState.ships of
                [ firstShip, secondShip ] ->
                    let
                        newFirstShip =
                            rotate direction firstShip

                        newGameState =
                            { gameState | ships = [ newFirstShip, secondShip ] }
                    in
                    ( newGameState, Cmd.none )

                _ ->
                    ( gameState, Cmd.none )

        Accelerate ->
            case gameState.ships of
                [ firstShip, secondShip ] ->
                    let
                        newFirstShip =
                            rocket_thrust firstShip

                        newGameState =
                            { gameState | ships = [ newFirstShip, secondShip ] }
                    in
                    ( newGameState, Cmd.none )

                _ ->
                    ( gameState, Cmd.none )

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
    { ships =
        [ { position = { x = 100, y = 100 }
          , velocity = { x = 0.0, y = 0.0 }
          , rotation = 0
          , crew = 5
          , energy = 100
          , shipType = Triangle
          , radius = 20
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
          , radius = 20
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
        }
    , timeElapsed = 0
    , space = { width = 800, height = 600 }
    }


subscribeToKeyPresses : Sub FrontendMsg
subscribeToKeyPresses =
    Browser.Events.onKeyDown (Decode.map keyPressed keyDecoder)
        |> Sub.map GameMsg


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


keyPressed : String -> GameMsg
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
