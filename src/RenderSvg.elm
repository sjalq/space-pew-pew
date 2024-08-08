module RenderSvg exposing (..)

import Html exposing (Html)
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


renderBodies : Table Body -> Svg GameMsg
renderBodies bodies =
    g [] (bodies |> Table.toList |> List.map renderBody)


renderBody : Body -> Svg GameMsg
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
    circle
        [ SvgAttr.cx (String.fromFloat planet.position.x)
        , SvgAttr.cy (String.fromFloat planet.position.y)
        , SvgAttr.r (String.fromFloat planet.radius)
        , SvgAttr.fill "gray"
        ]
        []



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



--renderShipSvg : Ship -> Svg AltGameMsg


renderShipSvg ship =
    let
        rotation =
            case ship.bodyType of
                Ship ship_ ->
                    ship_.rotation

                _ ->
                    0
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
            , SvgAttr.fill "#4a4a4a"
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
            , SvgAttr.fill "#6a6a6a"
            , SvgAttr.stroke "#2a2a2a"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , -- Right Wing
          path
            [ SvgAttr.d "M-10 -10 L-25 -25 L-10 -20 Z"
            , SvgAttr.fill "#6a6a6a"
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
