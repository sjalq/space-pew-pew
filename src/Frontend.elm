module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import L
import Lamdera
import Physics exposing (..)
import RenderSvg exposing (renderGame)
import Svg exposing (..)
import Table exposing (insertMaybe)
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
      , gameState = initState 0
      , gameCount = 0
      , pewsPewed = 0
      }
    , L.sendToBackend NewGameStarted
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

        NewGame ->
            ( { model | gameState = initState model.gameCount }, L.sendToBackend NewGameStarted )


updateGame : GameMsg -> GameState -> ( GameState, Cmd FrontendMsg )
updateGame msg gameState =
    case msg of
        FrameTick time ->
            let
                newBodies =
                    gameState.bodies
                        |> applyGravityToAll
                        |> updateLifetimes
                        |> Table.map (\body -> move gameState.space body)

                finalBodies =
                    performCollisions newBodies
                        |> Table.filter (not << projectile_destroyed)
                        |> Table.filter (not << ship_destroyed)

                newGameState =
                    { gameState
                        | bodies = finalBodies
                        , timeElapsed = gameState.timeElapsed + moment
                    }
            in
            ( newGameState, Cmd.none )

        FireProjectile shipId ->
            let
                -- find the ship
                -- generate the appropriate projectile
                ship =
                    gameState.bodies
                        |> Table.get shipId

                newProjectile =
                    ship
                        |> Maybe.andThen ship_fireProjectile

                newBodies =
                    Table.insertMaybe newProjectile gameState.bodies

                newState =
                    { gameState | bodies = newBodies }
            in
            ( newState, L.sendToBackend PewPewed )

        Rotate direction bodyId ->
            let
                ship =
                    gameState.bodies
                        |> Table.get bodyId
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
                        |> Table.get bodyId
                        |> Maybe.map ship_propel
            in
            ( { gameState
                | bodies =
                    gameState.bodies
                        |> Table.insertMaybe bodyToAccelerate
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

        GlobalUpdate global ->
            ( { model | gameCount = global.gameCount, pewsPewed = global.pewsPewed }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        let
            oldGameState =
                model.gameState

            gameState =
                { oldGameState
                    | entropyCount = model.gameCount
                }
        in
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div [ Attr.style "display" "flex", Attr.style "justify-content" "space-between" ]
                [ drawKeyboardLayoutLeft
                , Html.div
                    [ Attr.style "font-family" "sans-serif"
                    , Attr.style "padding-top" "40px"
                    , Attr.style "flex-grow" "1"
                    ]
                    [ renderGame gameState |> htmlGameMsg
                    ]
                , drawKeyboardLayoutRight
                ]
            ]
        , Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "20px" ]
            [ Html.button
                [ Attr.style "font-size" "16px"
                , Attr.style "padding" "10px 20px"
                , Html.Events.onClick NewGame
                ]
                [ Html.text "New Game" ]
            , Html.div []
                [ Html.text ("Game Count: " ++ String.fromInt model.gameCount)
                , Html.br [] []
                , Html.text ("Pews Pewed: " ++ String.fromInt model.pewsPewed)
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


initState : Int -> GameState
initState gameCount =
    { bodies =
        [ { id = 1
          , mass = 100000000000
          , position = { x = 600, y = 350 }
          , velocity = { x = 0, y = 0 }
          , radius = 50
          , bodyType = Planet { gravity = 9.8 }
          }
        , { id = 2 + gameCount
          , mass = 50
          , position = { x = 100, y = 100 }
          , velocity = { x = 0, y = 0 }
          , radius = 50
          , bodyType =
                Ship
                    { rotation = 0
                    , propulsion = Newtonian { thrust = 1 }
                    , rotationSpeed = 0.1
                    , projectile = Kenetic { damage = 1, lifetime = 1000, initialSpeed = 1, hit = False }
                    , crew = 30
                    }
          }
        , { id = 3 + gameCount
          , mass = 50
          , position = { x = 700, y = 500 }
          , velocity = { x = 0, y = 0 }
          , radius = 50
          , bodyType =
                Ship
                    { rotation = 0
                    , propulsion = LittleGrayMenTech { movementIncrement = 20 }
                    , rotationSpeed = 0.1
                    , projectile = Kenetic { damage = 1, lifetime = 1000, initialSpeed = 1, hit = False }
                    , crew = 30
                    }
          }
        ]
            |> Table.fromList
    , timeElapsed = 0
    , space = { width = 1200, height = 700 }
    , entropyCount = 0
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



--   MELEE CONTROLS SUMMARY
-- Top Player
-- E:                       Thrust
-- S and F:                 Steer
-- Q:                       Fire Primary Weapon
-- A:                       Fire Secondary Weapon
-- Bottom Player
-- UP or ENTER:             Thrust
-- LEFT and RIGHT:          Steer
-- RIGHT SHIFT:             Fire Primary Weapon
-- RIGHT CTRL:              Fire Secondary Weapon


keyPressed : String -> GameMsg
keyPressed key =
    case key of
        "w" ->
            Propel 2

        "a" ->
            Rotate Left 2

        "d" ->
            Rotate Right 2

        "f" ->
            FireProjectile 2

        "g" ->
            FireProjectile 2

        "ArrowUp" ->
            Propel 3

        "ArrowLeft" ->
            Rotate Left 3

        "ArrowRight" ->
            Rotate Right 3

        "Shift" ->
            FireProjectile 3

        "Control" ->
            FireProjectile 3

        _ ->
            let
                _ =
                    Debug.log "keyPressed" key
            in
            NoAction


drawKey : String -> String -> Html msg
drawKey key action =
    div [ Attr.style "margin-bottom" "10px" ]
        [ div [ Attr.style "border" "1px solid black", Attr.style "padding" "5px", Attr.style "display" "inline-block" ]
            [ Html.text key ]
        , Html.text ("-" ++ action)
        ]


drawKeyboardLayoutLeft : Html FrontendMsg
drawKeyboardLayoutLeft =
    div [ Attr.style "text-align" "center", Attr.style "font-family" "Arial, sans-serif", Attr.style "margin-left" "50px" ]
        [ h2 [ Attr.style "margin-bottom" "20px" ] [ Html.text "Left Player Controls" ]
        , div [ Attr.style "display" "flex", Attr.style "justify-content" "center" ]
            [ div [ Attr.style "margin" "10px", Attr.style "text-align" "left" ]
                [ drawKey "W" "Propel"
                , drawKey "A" "Rotate Left"
                , drawKey "D" "Rotate Right"
                , drawKey "F" "Fire Primary Weapon"
                , drawKey "G" "Fire Secondary Weapon"
                ]
            ]
        ]


drawKeyboardLayoutRight : Html FrontendMsg
drawKeyboardLayoutRight =
    div [ Attr.style "text-align" "center", Attr.style "font-family" "Arial, sans-serif", Attr.style "margin-right" "50px" ]
        [ h2 [ Attr.style "margin-bottom" "20px" ] [ Html.text "Right Player Controls" ]
        , div [ Attr.style "display" "flex", Attr.style "justify-content" "center" ]
            [ div [ Attr.style "margin" "10px", Attr.style "text-align" "left" ]
                [ drawKey "↑" "Propel"
                , drawKey "←" "Rotate Left"
                , drawKey "→" "Rotate Right"
                , drawKey "Shift" "Fire Primary Weapon"
                , drawKey "Control" "Fire Secondary Weapon"
                ]
            ]
        ]
