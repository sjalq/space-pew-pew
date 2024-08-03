module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Lamdera
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
    Time.every (1000 / 24) (\time -> GameMsg (FrameTick time))


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
                    model.gameState.ships |> List.map (updatePosition time)

                newProjectiles =
                    model.gameState.projectiles |> List.map (updatePosition time)

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

        FireProjectile projectile ->
            ( model, Cmd.none )

        MoveShip ship acceleration ->
            ( model, Cmd.none )


updatePosition time thing =
    let
        newX = (thing.position.x + thing.velocity.x * moment) |> wrapCoordinate 800
        newY = (thing.position.y + thing.velocity.y * moment) |> wrapCoordinate 600
    in
    { thing
        | position =
            { x = newX
            , y = newY
            }
    }


wrapCoordinate : Float -> Float -> Float
wrapCoordinate max value =
    if value < 0 then
        max + (value |> round |> modBy (round max) |> toFloat)
    else if value > max then
        value |> round |> modBy (round max) |> toFloat
    else
        value


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
          , velocity = { x = 0.01, y = -0.01 }
          , rotation = 2
          , crew = 5
          , energy = 100
          , shipType = Triangle
          }
        , { position = { x = 700, y = 500 }
          , velocity = { x = 0, y = 0 }
          , rotation = 3.14 -- Approximately PI, facing the opposite direction
          , crew = 3
          , energy = 80
          , shipType = Triangle
          }
        ]
    , projectiles = [] -- No projectiles (pew pew) as requested
    , planet =
        { position = { x = 400, y = 300 }
        , radius = 50
        , gravity = 9.8
        }
    , timeElapsed = 0
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
                [ "0,-10" -- nose
                , "5,10" -- right corner
                , "-5,10" -- left corner
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
