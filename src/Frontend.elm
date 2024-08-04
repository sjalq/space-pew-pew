module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Lamdera
import Physics exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
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
            model |> updateGame gameMsg


updateGame : GameMsg -> Model -> ( Model, Cmd FrontendMsg )
updateGame msg model =
    case msg of
        FrameTick time ->
            --update the position of the ships and based on their velocity
            let
                newShips =
                    model.gameState.ships |> List.map (move model.gameState.space)

                newProjectiles =
                    model.gameState.projectiles |> List.map (move model.gameState.space)

                oldGameState =
                    model.gameState

                newGameState =
                    { oldGameState
                        | ships = newShips
                        , projectiles = newProjectiles
                        , timeElapsed = oldGameState.timeElapsed + moment
                    }
            in
            ( { model | gameState = newGameState }, Cmd.none )

        FireProjectile ->
            ( model, Cmd.none )

        Rotate direction ->
            case model.gameState.ships of
                [firstShip, secondShip] ->
                    let
                        newFirstShip =
                            rotate direction firstShip

                        oldGameState =
                            model.gameState

                        newGameState =
                            { oldGameState | ships = [newFirstShip, secondShip] }

                        newModel =
                            { model | gameState = newGameState }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Accelerate ->
            case model.gameState.ships of
                [firstShip, secondShip ] ->
                    let
                        newFirstShip =
                            thrust firstShip

                        oldGameState =
                            model.gameState

                        newGameState =
                            { oldGameState | ships = [newFirstShip, secondShip] }

                        newModel =
                            { model | gameState = newGameState }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoAction ->
            ( model, Cmd.none )


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



-- Rendering function


renderGame : GameState -> Html GameMsg
renderGame gameState =
    let
        gameWidth =
            800

        gameHeight =
            600
    in
    svg
        [ SvgAttr.width (String.fromInt gameWidth)
        , SvgAttr.height (String.fromInt gameHeight)
        , SvgAttr.viewBox ("0 0 " ++ String.fromInt gameWidth ++ " " ++ String.fromInt gameHeight)
        , SvgAttr.style "background-color: black" -- Add a black background to represent space
        ]
        [ renderPlanet gameState.planet
        , renderShips gameState.ships
        , renderProjectiles gameState.projectiles
        ]


renderPlanet : Planet -> Svg GameMsg
renderPlanet planet =
    circle
        [ SvgAttr.cx (String.fromFloat planet.position.x)
        , SvgAttr.cy (String.fromFloat planet.position.y)
        , SvgAttr.r (String.fromFloat planet.radius)
        , SvgAttr.fill "gray"
        ]
        []


renderShips : List Ship -> Svg GameMsg
renderShips ships =
    g [] (List.map renderShip ships)


renderShip : Ship -> Svg GameMsg
renderShip ship =
    let
        shipSize =
            20

        shipPoints =
            String.join " "
                [ "10,0" -- nose (rotated right by 90 degrees)
                , "-10,5" -- right corner (now bottom corner)
                , "-10,-5" -- left corner (now top corner)
                ]
    in
    g
        [ SvgAttr.transform
            ("translate("
                ++ String.fromFloat ship.position.x
                ++ ","
                ++ String.fromFloat ship.position.y
                ++ ") "
                ++ "rotate("
                ++ String.fromFloat (ship.rotation * 180 / pi)
                ++ ")"
            )
        ]
        [ polygon
            [ SvgAttr.points shipPoints
            , SvgAttr.fill (shipColorFromType ship.shipType)
            , SvgAttr.stroke "white"
            , SvgAttr.strokeWidth "1"
            ]
            []
        ]


shipColorFromType : ShipType -> String
shipColorFromType shipType =
    case shipType of
        Triangle ->
            "blue"


renderProjectiles : List Projectile -> Svg GameMsg
renderProjectiles projectiles =
    g [] (List.map renderProjectile projectiles)


renderProjectile : Projectile -> Svg GameMsg
renderProjectile projectile =
    circle
        [ SvgAttr.cx (String.fromFloat projectile.position.x)
        , SvgAttr.cy (String.fromFloat projectile.position.y)
        , SvgAttr.r "2"
        , SvgAttr.fill "yellow"
        ]
        []


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
