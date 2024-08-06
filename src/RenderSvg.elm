module RenderSvg exposing (..)

import Html exposing (Html)
import Physics exposing (isPlanet, isProjectile, isShip)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Types exposing (..)



-- Rendering functions


renderGame : AltGameState -> Html AltGameMsg
renderGame gameState =
    let
        gameWidth =
            800

        gameHeight =
            600

        ships =
            gameState.bodies |> List.filter isShip

        planet =
            gameState.bodies |> List.filter isPlanet |> List.head

        projectiles =
            gameState.bodies |> List.filter isProjectile
    in
    svg
        [ SvgAttr.width (String.fromInt gameWidth)
        , SvgAttr.height (String.fromInt gameHeight)
        , SvgAttr.viewBox ("0 0 " ++ String.fromInt gameWidth ++ " " ++ String.fromInt gameHeight)
        , SvgAttr.style "background-color: black" -- Add a black background to represent space
        ]
        [ Maybe.map renderPlanet planet |> Maybe.withDefault (g [] [])
        , renderShips ships
        , renderProjectiles projectiles
        ]



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


renderShip : Ship -> Svg AltGameMsg
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
                AltShip ship_ ->
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
