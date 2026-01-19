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
import RenderWebGL exposing (renderGame, renderModelViewer)
import Html.Events exposing (onInput)
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
        [ Time.every moment (\time -> FEGameMsg (FrameTick time))
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
      , viewMode = GameView
      , modelViewer = 
          { selectedObject = ModelHumanShip
          , rotationX = 0
          , rotationY = 0
          , rotationZ = 0
          , zoom = 300
          , autoRotate = True
          , wireframe = False
          , colorR = 0.8
          , colorG = 0.8
          , colorB = 0.85
          , useCustomColor = False
          }
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

        FEGameMsg gameMsg ->
            let
                ( newGameState, cmd ) =
                    model.gameState |> GameLoop.updateGame gameMsg

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
        
        ToggleViewMode ->
            ( { model | viewMode = 
                case model.viewMode of
                    GameView -> ModelViewerView
                    ModelViewerView -> GameView 
              }
            , Cmd.none )
        
        SelectModel obj ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | selectedObject = obj } }, Cmd.none )
        
        UpdateRotationX value ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | rotationX = value } }, Cmd.none )
        
        UpdateRotationY value ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | rotationY = value } }, Cmd.none )
        
        UpdateRotationZ value ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | rotationZ = value } }, Cmd.none )
        
        UpdateZoom value ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | zoom = value } }, Cmd.none )
        
        ToggleAutoRotate ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | autoRotate = not viewer.autoRotate } }, Cmd.none )
        
        ToggleWireframe ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | wireframe = not viewer.wireframe } }, Cmd.none )
        
        ResetModelViewer ->
            ( { model | modelViewer = 
                { selectedObject = model.modelViewer.selectedObject
                , rotationX = 0
                , rotationY = 0
                , rotationZ = 0
                , zoom = 300
                , autoRotate = True
                , wireframe = False
                , colorR = 0.8
                , colorG = 0.8
                , colorB = 0.85
                , useCustomColor = False
                }
              }
            , Cmd.none )
        
        UpdateColorR value ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | colorR = value } }, Cmd.none )
        
        UpdateColorG value ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | colorG = value } }, Cmd.none )
        
        UpdateColorB value ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | colorB = value } }, Cmd.none )
        
        ToggleCustomColor ->
            let
                viewer = model.modelViewer
            in
            ( { model | modelViewer = { viewer | useCustomColor = not viewer.useCustomColor } }, Cmd.none )


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
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "20px" ]
            [ Html.button 
                [ Html.Events.onClick ToggleViewMode
                , Attr.style "font-size" "16px"
                , Attr.style "padding" "10px 20px"
                , Attr.style "margin-bottom" "20px"
                ]
                [ Html.text (if model.viewMode == GameView then "Switch to Model Viewer" else "Back to Game") ]
            , case model.viewMode of
                GameView ->
                    viewGame model
                
                ModelViewerView ->
                    viewModelViewer model
            ]
        ]
    }


viewGame : Model -> Html FrontendMsg
viewGame model =
    let
        oldGameState =
            model.gameState

        gameState =
            { oldGameState
                | entropyCount = model.gameCount
            }
    in
    Html.div [ Attr.style "text-align" "center" ]
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


viewModelViewer : Model -> Html FrontendMsg
viewModelViewer model =
    Html.div []
        [ Html.div [ Attr.style "padding" "20px", Attr.style "background" "#f0f0f0", Attr.style "border-radius" "8px", Attr.style "margin" "20px auto", Attr.style "max-width" "900px" ]
            [ Html.h2 [] [ Html.text "3D Model Viewer" ]
            , Html.div [ Attr.style "display" "flex", Attr.style "gap" "20px" ]
                [ -- Controls panel
                  Html.div [ Attr.style "flex" "0 0 250px" ]
                    [ Html.h3 [] [ Html.text "Controls" ]
                    
                    -- Object selector
                    , Html.div [ Attr.style "margin-bottom" "15px" ]
                        [ Html.label [] [ Html.text "Select Model: " ]
                        , Html.select [ Html.Events.onInput (\s -> SelectModel (stringToModelObject s)) ]
                            [ Html.option [ Attr.value "HumanShip" ] [ Html.text "Human Fighter Ship" ]
                            , Html.option [ Attr.value "Saucer" ] [ Html.text "UFO Saucer" ]
                            , Html.option [ Attr.value "Planet" ] [ Html.text "Planet" ]
                            , Html.option [ Attr.value "Projectile" ] [ Html.text "Projectile" ]
                            ]
                        ]
                    
                    -- Rotation sliders
                    , Html.div [ Attr.style "margin-bottom" "10px" ]
                        [ Html.label [] [ Html.text ("Rotation X: " ++ String.fromFloat model.modelViewer.rotationX ++ "°") ]
                        , Html.br [] []
                        , Html.input 
                            [ Attr.type_ "range"
                            , Attr.min "-180"
                            , Attr.max "180"
                            , Attr.value (String.fromFloat model.modelViewer.rotationX)
                            , Html.Events.onInput (String.toFloat >> Maybe.withDefault 0 >> UpdateRotationX)
                            , Attr.style "width" "100%"
                            ] []
                        ]
                    
                    , Html.div [ Attr.style "margin-bottom" "10px" ]
                        [ Html.label [] [ Html.text ("Rotation Y: " ++ String.fromFloat model.modelViewer.rotationY ++ "°") ]
                        , Html.br [] []
                        , Html.input 
                            [ Attr.type_ "range"
                            , Attr.min "-180"
                            , Attr.max "180"
                            , Attr.value (String.fromFloat model.modelViewer.rotationY)
                            , Html.Events.onInput (String.toFloat >> Maybe.withDefault 0 >> UpdateRotationY)
                            , Attr.style "width" "100%"
                            ] []
                        ]
                    
                    , Html.div [ Attr.style "margin-bottom" "10px" ]
                        [ Html.label [] [ Html.text ("Rotation Z: " ++ String.fromFloat model.modelViewer.rotationZ ++ "°") ]
                        , Html.br [] []
                        , Html.input 
                            [ Attr.type_ "range"
                            , Attr.min "-180"
                            , Attr.max "180"
                            , Attr.value (String.fromFloat model.modelViewer.rotationZ)
                            , Html.Events.onInput (String.toFloat >> Maybe.withDefault 0 >> UpdateRotationZ)
                            , Attr.style "width" "100%"
                            ] []
                        ]
                    
                    -- Zoom slider
                    , Html.div [ Attr.style "margin-bottom" "10px" ]
                        [ Html.label [] [ Html.text ("Zoom: " ++ String.fromFloat model.modelViewer.zoom) ]
                        , Html.br [] []
                        , Html.input 
                            [ Attr.type_ "range"
                            , Attr.min "100"
                            , Attr.max "500"
                            , Attr.value (String.fromFloat model.modelViewer.zoom)
                            , Html.Events.onInput (String.toFloat >> Maybe.withDefault 300 >> UpdateZoom)
                            , Attr.style "width" "100%"
                            ] []
                        ]
                    
                    -- Color controls
                    , Html.div [ Attr.style "margin-top" "20px", Attr.style "padding-top" "20px", Attr.style "border-top" "1px solid #ccc" ]
                        [ Html.h4 [] [ Html.text "Color Customization" ]
                        , Html.div [ Attr.style "margin-bottom" "10px" ]
                            [ Html.button
                                [ Html.Events.onClick ToggleCustomColor
                                , Attr.style "padding" "5px 10px"
                                , Attr.style "width" "100%"
                                , Attr.style "background-color" (if model.modelViewer.useCustomColor then "#4CAF50" else "#f0f0f0")
                                , Attr.style "color" (if model.modelViewer.useCustomColor then "white" else "black")
                                ]
                                [ Html.text (if model.modelViewer.useCustomColor then "Custom Colors ON" else "Use Default Colors") ]
                            ]
                        
                        , if model.modelViewer.useCustomColor then
                            Html.div []
                                [ Html.div [ Attr.style "margin-bottom" "10px" ]
                                    [ Html.label [] [ Html.text ("Red: " ++ String.fromInt (round (model.modelViewer.colorR * 255))) ]
                                    , Html.br [] []
                                    , Html.input 
                                        [ Attr.type_ "range"
                                        , Attr.min "0"
                                        , Attr.max "1"
                                        , Attr.step "0.01"
                                        , Attr.value (String.fromFloat model.modelViewer.colorR)
                                        , Html.Events.onInput (String.toFloat >> Maybe.withDefault 0.5 >> UpdateColorR)
                                        , Attr.style "width" "100%"
                                        , Attr.style "background" ("linear-gradient(to right, black, red)")
                                        ] []
                                    ]
                                
                                , Html.div [ Attr.style "margin-bottom" "10px" ]
                                    [ Html.label [] [ Html.text ("Green: " ++ String.fromInt (round (model.modelViewer.colorG * 255))) ]
                                    , Html.br [] []
                                    , Html.input 
                                        [ Attr.type_ "range"
                                        , Attr.min "0"
                                        , Attr.max "1"
                                        , Attr.step "0.01"
                                        , Attr.value (String.fromFloat model.modelViewer.colorG)
                                        , Html.Events.onInput (String.toFloat >> Maybe.withDefault 0.5 >> UpdateColorG)
                                        , Attr.style "width" "100%"
                                        , Attr.style "background" ("linear-gradient(to right, black, green)")
                                        ] []
                                    ]
                                
                                , Html.div [ Attr.style "margin-bottom" "10px" ]
                                    [ Html.label [] [ Html.text ("Blue: " ++ String.fromInt (round (model.modelViewer.colorB * 255))) ]
                                    , Html.br [] []
                                    , Html.input 
                                        [ Attr.type_ "range"
                                        , Attr.min "0"
                                        , Attr.max "1"
                                        , Attr.step "0.01"
                                        , Attr.value (String.fromFloat model.modelViewer.colorB)
                                        , Html.Events.onInput (String.toFloat >> Maybe.withDefault 0.5 >> UpdateColorB)
                                        , Attr.style "width" "100%"
                                        , Attr.style "background" ("linear-gradient(to right, black, blue)")
                                        ] []
                                    ]
                                
                                -- Color preview
                                , Html.div 
                                    [ Attr.style "width" "100%"
                                    , Attr.style "height" "30px"
                                    , Attr.style "background-color" (rgbToHex model.modelViewer.colorR model.modelViewer.colorG model.modelViewer.colorB)
                                    , Attr.style "border" "1px solid #333"
                                    , Attr.style "border-radius" "4px"
                                    ]
                                    []
                                ]
                          else
                            Html.text ""
                        ]
                    
                    -- Toggle buttons
                    , Html.div [ Attr.style "margin-bottom" "10px" ]
                        [ Html.button
                            [ Html.Events.onClick ToggleAutoRotate
                            , Attr.style "padding" "5px 10px"
                            , Attr.style "margin-right" "10px"
                            ]
                            [ Html.text (if model.modelViewer.autoRotate then "Stop Auto-Rotate" else "Start Auto-Rotate") ]
                        
                        ]
                    
                    , Html.div []
                        [ Html.button
                            [ Html.Events.onClick ResetModelViewer
                            , Attr.style "padding" "5px 10px"
                            ]
                            [ Html.text "Reset View" ]
                        ]
                    ]
                
                -- 3D View
                , Html.div [ Attr.style "flex" "1" ]
                    [ renderModelViewer model.modelViewer model.gameCount
                    ]
                ]
            ]
        ]


stringToModelObject : String -> ModelObject
stringToModelObject str =
    case str of
        "HumanShip" -> ModelHumanShip
        "Saucer" -> ModelSaucer  
        "Planet" -> ModelPlanet
        "Projectile" -> ModelProjectile
        _ -> ModelHumanShip


rgbToHex : Float -> Float -> Float -> String
rgbToHex r g b =
    let
        toHexComponent : Float -> String
        toHexComponent value =
            let
                intValue = round (value * 255)
                hex = String.fromInt intValue
            in
            "rgb(" ++ String.fromInt (round (r * 255)) ++ ", " 
                   ++ String.fromInt (round (g * 255)) ++ ", " 
                   ++ String.fromInt (round (b * 255)) ++ ")"
    in
    "rgb(" ++ String.fromInt (round (r * 255)) ++ ", " 
           ++ String.fromInt (round (g * 255)) ++ ", " 
           ++ String.fromInt (round (b * 255)) ++ ")"


gameMsgToFrontendMsg : GameMsg -> FrontendMsg
gameMsgToFrontendMsg msg =
    FEGameMsg msg


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
            |> Sub.map FEGameMsg
        , Browser.Events.onKeyUp (Decode.map KeyReleased keyDecoder)
            |> Sub.map FEGameMsg
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


keyPressed : String -> GameMsg
keyPressed key =
    KeyPressed key


keyReleased : String -> GameMsg
keyReleased key =
    KeyReleased key


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
