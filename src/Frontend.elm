module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import GameLoop 
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
        [ keySubs
        , Time.every 10000 Ping
        , Time.every moment UpdateGame
        ]


keySubs : Sub FrontendMsg
keySubs =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyPressed keyDecoder)
            |> Sub.map Input
        , Browser.Events.onKeyUp (Decode.map KeyReleased keyDecoder)
            |> Sub.map Input
        ]


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , gameCount = 0
      , pewsPewed = 0
      , chatInput = ""
      , trollbox = []
      , gameState = GameLoop.initState 0 "_" "_"
      , lastPing = Time.millisToPosix 0
      , lastPong = Time.millisToPosix 0
      , emaPingTime = 0
      , depressedKeys = Set.empty
      }
    , L.sendToBackend StartNewGame
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

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        Input inputMsg ->
            let
                newModel =
                    { model | depressedKeys = model.depressedKeys |> GameLoop.updateInputs inputMsg }
            in
            ( newModel, Cmd.none )

        NewGame ->
            ( model, L.sendToBackend StartNewGame )

        SendChat ->
            ( { model | chatInput = "" }
            , L.sendToBackend (AddChat model.chatInput)
            )

        ChatInputChanged newInput ->
            ( { model | chatInput = newInput }, Cmd.none )

        Ping time ->
            ( { model | lastPing = time }, L.sendToBackend (PingBackend time) )

        PongWithTime time ->
            let
                ping =
                    model.lastPing |> Time.posixToMillis

                pong =
                    time |> Time.posixToMillis

                pingTime =
                    pong - ping

                newModel =
                    { model
                        | lastPong = time
                        , emaPingTime = (0.5 * (pingTime |> toFloat)) + (0.5 * model.emaPingTime)
                    }
            in
            ( { newModel | lastPong = time }, Cmd.none )

        UpdateGame _ ->
            let
                gameMsgs =
                    model.depressedKeys |> GameLoop.keysToGameMsgs
            in
            ( model
              --     { model
              --     | gameState =
              --         model.gameState
              --             |> GameLoop.updateMsg (FrameTick model.depressedKeys time)
              --   }
            , L.sendToBackend (SubmitGameMsgs gameMsgs)
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        UpdateGlobal global ->
            ( { model
                | gameCount = global.gameCount
                , pewsPewed = global.pewsPewed
                , trollbox = global.trollbox
              }
            , Cmd.none
            )

        UpdateGameState gameState ->
            ( { model | gameState = gameState }, Cmd.none )

        Pong ->
            ( model, L.performWithTime PongWithTime )


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
                , Html.br [] []
                , Html.text ("Ping Time : " ++ (model.emaPingTime |> round |> String.fromInt))
                ]
            ]
        ]
    }


htmlGameMsg : Html InputMsg -> Html FrontendMsg
htmlGameMsg =
    Html.map Input


listOfBodies model =
    model.gameState.ships ++ model.gameState.projectiles


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


keyPressed : String -> InputMsg
keyPressed key =
    KeyPressed key


keyReleased : String -> InputMsg
keyReleased key =
    KeyReleased key


drawKey : String -> String -> String -> Model -> Html msg
drawKey icon key action model =
    let
        isPressed =
            Set.member (String.toLower key) model.depressedKeys
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
        [ span [ Attr.style "font-weight" "bold" ] [ Html.text (String.left 6 msg.browserId ++ ": ") ]
        , Html.text msg.message
        ]
