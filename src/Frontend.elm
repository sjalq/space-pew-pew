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
import Set
import Svg exposing (..)
import Table exposing (insertMaybe)
import Task
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
      , chatInput = ""
      , trollbox = []
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

        SendChat ->
            ( { model | chatInput = "" }
            , L.sendToBackend (AddChat model.chatInput)
            )

        ChatInputChanged newInput ->
            ( { model | chatInput = newInput }, Cmd.none )


performNow : msg -> Cmd msg
performNow msg_ =
    Task.perform identity (Task.succeed msg_)


updateGame : GameMsg -> GameState -> ( GameState, Cmd FrontendMsg )
updateGame msg gameState =
    case msg of
        NoAction ->
            ( gameState, Cmd.none )

        FrameTick time ->
            let
                newBodies =
                    gameState.bodies
                        |> applyGravityToAll
                        |> updateLifetimes
                        |> Table.map (\body -> applyVelocity gameState.space body)

                finalBodies =
                    performCollisions newBodies
                        |> Table.filter (not << projectile_destroyed)
                        |> Table.filter (not << ship_destroyed)

                newGameState =
                    { gameState
                        | bodies = finalBodies
                        , timeElapsed = gameState.timeElapsed + moment
                    }

                performKeys =
                    gameState.depressedKeys
                        |> Set.toList
                        |> List.map keyToMsg
                        |> List.map GameMsg
                        |> List.map performNow
                        |> Cmd.batch
            in
            ( newGameState
            , performKeys
            )

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

        KeyPressed key ->
            ( { gameState | depressedKeys = Set.insert (String.toLower key) gameState.depressedKeys }, Cmd.none )

        KeyReleased key ->
            ( { gameState | depressedKeys = Set.remove (String.toLower key) gameState.depressedKeys }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        GlobalUpdate global ->
            ( { model
                | gameCount = global.gameCount
                , pewsPewed = global.pewsPewed
                , trollbox = global.trollbox
              }
            , Cmd.none
            )


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
                [ drawKeyboardLayoutLeft model
                , Html.div
                    [ Attr.style "font-family" "sans-serif"
                    , Attr.style "padding-top" "40px"
                    , Attr.style "flex-grow" "1"
                    ]
                    [ renderGame gameState |> htmlGameMsg
                    ]
                , Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "flex-direction" "column"
                    , Attr.style "align-items" "center"
                    ]
                    [ drawKeyboardLayoutRight model
                    , drawTrollbox model
                    ]
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
        , { id = 2
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
                    , maxCrew = 30
                    , crew = 30
                    }
          }
        , { id = 3
          , mass = 50
          , position = { x = 700, y = 500 }
          , velocity = { x = 0, y = 0 }
          , radius = 32
          , bodyType =
                Ship
                    { rotation = 0
                    , propulsion = LittleGrayMenTech { movementIncrement = 20 }
                    , rotationSpeed = tau / 32
                    , projectile = Kenetic { damage = 1, lifetime = 1000, initialSpeed = 1, hit = False }
                    , maxCrew = 10
                    , crew = 10
                    }
          }
        ]
            |> Table.fromList
    , timeElapsed = 0
    , space = { width = 1200, height = 700 }
    , entropyCount = gameCount
    , depressedKeys = Set.empty
    }


listOfBodies model =
    model.gameState.ships ++ model.gameState.projectiles


subscribeToKeyPresses : Sub FrontendMsg
subscribeToKeyPresses =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyPressed keyDecoder)
            |> Sub.map GameMsg
        , Browser.Events.onKeyUp (Decode.map KeyReleased keyDecoder)
            |> Sub.map GameMsg
        ]


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
    KeyPressed key


keyReleased : String -> GameMsg
keyReleased key =
    KeyReleased key


keyToMsg : String -> GameMsg
keyToMsg key =
    let
        key_ =
            String.toLower key
    in
    case key_ of
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

        "arrowup" ->
            Propel 3

        "arrowleft" ->
            Rotate Left 3

        "arrowright" ->
            Rotate Right 3

        "shift" ->
            FireProjectile 3

        "control" ->
            FireProjectile 3

        _ ->
            NoAction


drawKey : String -> String -> String -> Model -> Html msg
drawKey icon key action model =
    let
        isPressed =
            Set.member (String.toLower key) model.gameState.depressedKeys
    in
    div [ Attr.style "margin-bottom" "10px" ]
        [ div
            [ Attr.style "border" "1px solid black"
            , Attr.style "padding" "5px"
            , Attr.style "display" "inline-block"
            , if isPressed then
                Attr.style "background-color" "red"

              else
                Attr.style "background-color" "white"
            ]
            [ Html.text icon ]
        , Html.text ("-" ++ action)
        ]


drawKeyboardLayoutLeft : Model -> Html FrontendMsg
drawKeyboardLayoutLeft model =
    div [ Attr.style "text-align" "center", Attr.style "font-family" "Arial, sans-serif", Attr.style "margin-left" "50px" ]
        [ h2 [ Attr.style "margin-bottom" "20px" ] [ Html.text "Left Player Controls" ]
        , div [ Attr.style "display" "flex", Attr.style "justify-content" "center" ]
            [ div [ Attr.style "margin" "10px", Attr.style "text-align" "left" ]
                [ drawKey "W" "W" "Propel" model
                , drawKey "A" "A" "Rotate Left" model
                , drawKey "D" "D" "Rotate Right" model
                , drawKey "F" "F" "Fire Primary Weapon" model
                , drawKey "G" "G" "Fire Secondary Weapon" model
                ]
            ]
        ]


drawKeyboardLayoutRight : Model -> Html FrontendMsg
drawKeyboardLayoutRight model =
    div [ Attr.style "text-align" "center", Attr.style "font-family" "Arial, sans-serif", Attr.style "margin-right" "50px" ]
        [ h2 [ Attr.style "margin-bottom" "20px" ] [ Html.text "Right Player Controls" ]
        , div [ Attr.style "display" "flex", Attr.style "justify-content" "center" ]
            [ div [ Attr.style "margin" "10px", Attr.style "text-align" "left" ]
                [ drawKey "↑" "ArrowUp" "Propel" model
                , drawKey "←" "ArrowLeft" "Rotate Left" model
                , drawKey "→" "ArrowRight" "Rotate Right" model
                , drawKey "Shift" "Shift" "Fire Primary Weapon" model
                , drawKey "Control" "Control" "Fire Secondary Weapon" model
                ]
            ]
        ]


drawTrollbox : Model -> Html FrontendMsg
drawTrollbox model =
    div [ Attr.style "width" "300px", Attr.style "border" "1px solid #ccc", Attr.style "margin" "20px auto" ]
        [ div
            [ Attr.style "height" "300px"
            , Attr.style "overflow-y" "scroll"
            , Attr.style "padding" "10px"
            , Attr.style "background-color" "#f0f0f0"
            ]
            (List.map drawChatMessage (List.reverse model.trollbox))
        , div [ Attr.style "display" "flex", Attr.style "padding" "10px" ]
            [ input
                [ Attr.type_ "text"
                , Attr.placeholder "Type your message..."
                , Attr.value model.chatInput
                , Attr.style "flex-grow" "1"
                , Attr.style "margin-right" "10px"
                , Html.Events.onInput ChatInputChanged
                ]
                []
            , Html.button [ Html.Events.onClick SendChat, Attr.style "padding" "5px 10px" ]
                [ Html.text "Send" ]
            ]
        ]


drawChatMessage : ChatMessage -> Html FrontendMsg
drawChatMessage msg =
    div [ Attr.style "margin-bottom" "5px" ]
        [ span [ Attr.style "font-weight" "bold" ] [ Html.text (String.left 6 msg.clientId ++ ": ") ]
        , Html.text msg.message
        ]
