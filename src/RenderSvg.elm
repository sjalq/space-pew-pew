module RenderSvg exposing (..)

import Html exposing (Html)
import Sha256 exposing (sha256)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Table exposing (Table)
import Types exposing (..)



-- Rendering functions


renderGame : GameState -> Html GameMsg
renderGame gameState =
    let
        gameWidth =
            gameState.space.width |> round

        gameHeight =
            gameState.space.height |> round
    in
    svg
        [ SvgAttr.width (String.fromInt gameWidth)
        , SvgAttr.height (String.fromInt gameHeight)
        , SvgAttr.viewBox ("0 0 " ++ String.fromInt gameWidth ++ " " ++ String.fromInt gameHeight)
        , SvgAttr.style "background-color: black" -- Add a black background to represent space
        ]
        [ renderBodies gameState.bodies
        ]


renderBodies bodies =
    g [] (bodies |> Table.toList |> List.map renderBody)


renderBody body =
    case body.bodyType of
        Planet _ ->
            renderPlanet body

        Ship _ ->
            renderShipSvg body

        Projectile _ ->
            renderProjectile body

        _ ->
            text ""



--renderPlanet : Planet -> Svg AltGameMsg


renderPlanet planet =
    g []
        [ -- Background circle (Jupiter's main body)
          circle
            [ SvgAttr.cx (String.fromFloat planet.position.x)
            , SvgAttr.cy (String.fromFloat planet.position.y)
            , SvgAttr.r (String.fromFloat planet.radius)
            , SvgAttr.fill "#E0A868"
            ]
            []
        , -- Great Red Spot
          ellipse
            [ SvgAttr.cx (String.fromFloat (planet.position.x + 15))
            , SvgAttr.cy (String.fromFloat (planet.position.y + 20))
            , SvgAttr.rx (String.fromFloat (planet.radius * 0.25))
            , SvgAttr.ry (String.fromFloat (planet.radius * 0.15))
            , SvgAttr.fill "#C1440E"
            , SvgAttr.transform ("rotate(-15, " ++ String.fromFloat planet.position.x ++ ", " ++ String.fromFloat planet.position.y ++ ")")
            ]
            []
        , -- Bands
          path
            [ SvgAttr.d
                ("M"
                    ++ String.fromFloat (planet.position.x - planet.radius * 0.9)
                    ++ " "
                    ++ String.fromFloat planet.position.y
                    ++ " Q "
                    ++ String.fromFloat planet.position.x
                    ++ " "
                    ++ String.fromFloat (planet.position.y - planet.radius * 0.3)
                    ++ " "
                    ++ String.fromFloat (planet.position.x + planet.radius * 0.9)
                    ++ " "
                    ++ String.fromFloat planet.position.y
                )
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#D39553"
            , SvgAttr.strokeWidth (String.fromFloat (planet.radius * 0.1))
            ]
            []
        , path
            [ SvgAttr.d
                ("M"
                    ++ String.fromFloat (planet.position.x - planet.radius * 0.9)
                    ++ " "
                    ++ String.fromFloat (planet.position.y + planet.radius * 0.2)
                    ++ " Q "
                    ++ String.fromFloat planet.position.x
                    ++ " "
                    ++ String.fromFloat (planet.position.y + planet.radius * 0.5)
                    ++ " "
                    ++ String.fromFloat (planet.position.x + planet.radius * 0.9)
                    ++ " "
                    ++ String.fromFloat (planet.position.y + planet.radius * 0.2)
                )
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#C17F3E"
            , SvgAttr.strokeWidth (String.fromFloat (planet.radius * 0.12))
            ]
            []
        , path
            [ SvgAttr.d
                ("M"
                    ++ String.fromFloat (planet.position.x - planet.radius * 0.9)
                    ++ " "
                    ++ String.fromFloat (planet.position.y - planet.radius * 0.2)
                    ++ " Q "
                    ++ String.fromFloat planet.position.x
                    ++ " "
                    ++ String.fromFloat (planet.position.y - planet.radius * 0.4)
                    ++ " "
                    ++ String.fromFloat (planet.position.x + planet.radius * 0.9)
                    ++ " "
                    ++ String.fromFloat (planet.position.y - planet.radius * 0.2)
                )
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#E8B77D"
            , SvgAttr.strokeWidth (String.fromFloat (planet.radius * 0.08))
            ]
            []
        , path
            [ SvgAttr.d
                ("M"
                    ++ String.fromFloat (planet.position.x - planet.radius * 0.9)
                    ++ " "
                    ++ String.fromFloat (planet.position.y + planet.radius * 0.4)
                    ++ " Q "
                    ++ String.fromFloat planet.position.x
                    ++ " "
                    ++ String.fromFloat (planet.position.y + planet.radius * 0.3)
                    ++ " "
                    ++ String.fromFloat (planet.position.x + planet.radius * 0.9)
                    ++ " "
                    ++ String.fromFloat (planet.position.y + planet.radius * 0.4)
                )
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#A66A2E"
            , SvgAttr.strokeWidth (String.fromFloat (planet.radius * 0.06))
            ]
            []
        ]



--renderShips : List Ship -> Svg AltGameMsg


renderShips ships =
    g [] (List.map renderShipSvg ships)



--renderProjectiles : List Projectile -> Svg AltGameMsg


renderProjectiles projectiles =
    g [] (List.map renderProjectile projectiles)



--renderProjectile : Projectile -> Svg AltGameMsg


renderProjectile projectile =
    circle
        [ SvgAttr.cx (String.fromFloat projectile.position.x)
        , SvgAttr.cy (String.fromFloat projectile.position.y)
        , SvgAttr.r "2"
        , SvgAttr.fill "yellow"
        ]
        []


hashToColor : Int -> String
hashToColor id =
    let
        hash =
            id
                |> String.fromInt
                |> sha256
                |> String.left 6

        r =
            String.slice 0 2 hash

        g =
            String.slice 2 4 hash

        b =
            String.slice 4 6 hash
    in
    "#" ++ r ++ g ++ b


renderShipSvg ship =
    let
        rotation =
            case ship.bodyType of
                Ship ship_ ->
                    ship_.rotation

                _ ->
                    0

        wingColor =
            hashToColor ship.id

        bodyColor =
            hashToColor ship.id
    in
    g
        [ SvgAttr.transform
            ("translate("
                ++ String.fromFloat ship.position.x
                ++ ","
                ++ String.fromFloat ship.position.y
                ++ ") "
                ++ "rotate("
                ++ String.fromFloat (rotation * 180 / pi)
                ++ ")"
            )
        ]
        [ -- Shield radius
          circle
            [ SvgAttr.cx "0"
            , SvgAttr.cy "0"
            , SvgAttr.r (String.fromFloat ship.radius)
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#00ffff"
            , SvgAttr.strokeWidth "1"
            , SvgAttr.opacity "0.5"
            ]
            []
        , -- Ship body
          path
            [ SvgAttr.d "M30 0 L-30 20 L-30 -20 Z"
            , SvgAttr.fill bodyColor
            , SvgAttr.stroke "#2a2a2a"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , -- Cockpit
          circle
            [ SvgAttr.cx "10"
            , SvgAttr.cy "0"
            , SvgAttr.r "8"
            , SvgAttr.fill "#7bb7e0"
            , SvgAttr.stroke "#2a2a2a"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , -- Left Wing
          path
            [ SvgAttr.d "M-10 10 L-25 25 L-10 20 Z"
            , SvgAttr.fill wingColor
            , SvgAttr.stroke "#2a2a2a"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , -- Right Wing
          path
            [ SvgAttr.d "M-10 -10 L-25 -25 L-10 -20 Z"
            , SvgAttr.fill wingColor
            , SvgAttr.stroke "#2a2a2a"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , -- Engine
          rect
            [ SvgAttr.x "-35"
            , SvgAttr.y "-7"
            , SvgAttr.width "10"
            , SvgAttr.height "14"
            , SvgAttr.fill "#c33"
            , SvgAttr.stroke "#2a2a2a"
            , SvgAttr.strokeWidth "2"
            ]
            []
        ]
