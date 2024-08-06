module RenderSvg exposing (..)

import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Types exposing (..)
import Html exposing (Html)

-- Rendering functions


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
