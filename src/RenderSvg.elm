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

        ( ship1, ship2 ) =
            getFirstTwoShips gameState.bodies
    in
    svg
        [ SvgAttr.width (String.fromInt gameWidth)
        , SvgAttr.height (String.fromInt gameHeight)
        , SvgAttr.viewBox ("0 0 " ++ String.fromInt gameWidth ++ " " ++ String.fromInt gameHeight)
        , SvgAttr.style "background-color: black" -- Add a black background to represent space
        ]
        [ ship1 |> Maybe.map (drawCrewHealthBar { x = 10, y = (toFloat gameHeight / 2) - 25 }) |> Maybe.withDefault (text "")
        , ship2 |> Maybe.map (drawCrewHealthBar { x = (toFloat gameWidth - 54), y = (toFloat gameHeight / 2) - 25 }) |> Maybe.withDefault (text "")
        , gameState.bodies |> renderBodies
        ]


body_isShip : Body -> Bool
body_isShip body =
    case body.bodyType of
        Ship _ ->
            True

        _ ->
            False


getFirstTwoShips : Table Body -> ( Maybe Body, Maybe Body )
getFirstTwoShips bodies =
    case bodies |> Table.filter body_isShip |> Table.toList of
        [ ship1, ship2 ] ->
            ( Just ship1, Just ship2 )

        _ ->
            ( Nothing, Nothing )


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


colorA ship =
    hashToColor ship.id


colorB ship =
    hashToColor ship.id


rotation ship =
    case ship.bodyType of
        Ship ship_ ->
            ship_.rotation

        _ ->
            0


renderShipSvg ship =
    case ship.bodyType of
        Ship ship_ ->
            case ship_.propulsion of
                LittleGrayMenTech _ ->
                    renderLittleGrayMenTechShip ship

                _ ->
                    renderHumanShip ship

        _ ->
            text ""


renderLittleGrayMenTechShip ship =
    g
        [ SvgAttr.transform
            ("translate("
                ++ String.fromFloat ship.position.x
                ++ ","
                ++ String.fromFloat ship.position.y
                ++ ")"
                ++ "rotate("
                ++ String.fromFloat (rotation ship * 180 / pi)
                ++ ")"
            )
        ]
        [ defs
            []
            [ radialGradient
                [ SvgAttr.id "saucerGradient"
                , SvgAttr.cx "50%"
                , SvgAttr.cy "50%"
                , SvgAttr.r "50%"
                , SvgAttr.fx "50%"
                , SvgAttr.fy "50%"
                ]
                [ stop [ SvgAttr.offset "0%", SvgAttr.stopColor "#e0e0e0", SvgAttr.stopOpacity "1" ] []
                , stop [ SvgAttr.offset "70%", SvgAttr.stopColor "#a0a0a0", SvgAttr.stopOpacity "1" ] []
                , stop [ SvgAttr.offset "100%", SvgAttr.stopColor "#808080", SvgAttr.stopOpacity "1" ] []
                ]
            ]
        , circle
            [ SvgAttr.cx "0"
            , SvgAttr.cy "0"
            , SvgAttr.r (String.fromFloat ship.radius)
            , SvgAttr.fill "url(#saucerGradient)"
            , SvgAttr.stroke (colorA ship)
            , SvgAttr.strokeWidth "2"
            ]
            []
        , circle
            [ SvgAttr.cx "0"
            , SvgAttr.cy "0"
            , SvgAttr.r (String.fromFloat (ship.radius / 3))
            , SvgAttr.fill "#4a90e2"
            , SvgAttr.stroke (colorB ship)
            , SvgAttr.strokeWidth "2"
            ]
            []
        , List.map
            (\( x, y, color ) ->
                circle
                    [ SvgAttr.cx (String.fromFloat (x * ship.radius / 100 - ship.radius))
                    , SvgAttr.cy (String.fromFloat (y * ship.radius / 100 - ship.radius))
                    , SvgAttr.r (String.fromFloat (ship.radius * 0.05))
                    , SvgAttr.fill color
                    ]
                    []
            )
            [ ( 25, 100, "#ffff00" )
            , ( 175, 100, "#ff0000" )
            , ( 100, 25, "#ffff00" )
            , ( 100, 175, "#ffff00" )
            , ( 50, 50, "#ffff00" )
            , ( 150, 50, "#ffff00" )
            , ( 50, 150, "#ffff00" )
            , ( 150, 150, "#ffff00" )
            ]
            |> g []
        ]


renderHumanShip ship =
    g
        [ SvgAttr.transform
            ("translate("
                ++ String.fromFloat ship.position.x
                ++ ","
                ++ String.fromFloat ship.position.y
                ++ ") "
                ++ "rotate("
                ++ String.fromFloat (rotation ship * 180 / pi)
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
            , SvgAttr.fill (colorA ship)
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
            , SvgAttr.fill (colorB ship)
            , SvgAttr.stroke "#2a2a2a"
            , SvgAttr.strokeWidth "2"
            ]
            []
        , -- Right Wing
          path
            [ SvgAttr.d "M-10 -10 L-25 -25 L-10 -20 Z"
            , SvgAttr.fill (colorB ship)
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


drawCrewHealthBar : Vector2D -> Body -> Svg GameMsg
drawCrewHealthBar position ship =
    case ship.bodyType of
        Ship ship_ ->
            let
                drawGreenBlock index =
                    rect
                        [ SvgAttr.width (String.fromFloat blockWidth)
                        , SvgAttr.height (String.fromFloat blockHeight)
                        , SvgAttr.x "0"
                        , SvgAttr.y (String.fromFloat (toFloat index * blockHeight))
                        , SvgAttr.fill "#00FF00"
                        , SvgAttr.stroke "#008000"
                        , SvgAttr.strokeWidth "1"
                        ]
                        []

                blockWidth =
                    20

                blockHeight =
                    5

                rows =
                    ship_.maxCrew // 2

                leftColumnCount =
                    ship_.crew // 2

                rightColumnCount =
                    ship_.crew - leftColumnCount

                leftBlocks =
                    List.range -(leftColumnCount - 1) 0 |> List.map drawGreenBlock

                rightBlocks =
                    List.range -(rightColumnCount - 1) 0 |> List.map drawGreenBlock

                drawColumn x blocks =
                    g [ SvgAttr.transform ("translate(" ++ String.fromFloat x ++ ", 0)") ]
                        (List.indexedMap (\i block -> g [ SvgAttr.transform ("translate(0, " ++ String.fromFloat (toFloat i * (blockHeight + 2)) ++ ")") ] [ block ]) blocks)
            in
            g
                [ SvgAttr.transform
                    ("translate("
                        ++ String.fromFloat (position.x)
                        ++ ","
                        ++ String.fromFloat (position.y)
                        ++ ")"
                    )
                ]
                [ drawColumn 0 leftBlocks
                , drawColumn (blockWidth + 4) rightBlocks
                ]

        _ ->
            g [] []
