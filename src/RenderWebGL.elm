module RenderWebGL exposing (renderGame, renderModelViewer)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Settings exposing (Setting)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import Table exposing (Table)
import Types exposing (..)
import Sha256 exposing (sha256)
import Set


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    }


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , model : Mat4
    , lightDirection : Vec3
    , ambientColor : Vec3
    , diffuseColor : Vec3
    , specularColor : Vec3
    , shininess : Float
    , opacity : Float
    , time : Float
    }


type alias SimpleUniforms =
    { perspective : Mat4
    , camera : Mat4
    , model : Mat4
    , uniformColor : Vec3
    , opacity : Float
    }


renderGame : GameState -> Html GameMsg
renderGame gameState =
    let
        gameWidth =
            gameState.space.width |> round

        gameHeight =
            gameState.space.height |> round

        ( ship1, ship2 ) =
            getFirstTwoShips gameState.bodies

        time =
            toFloat (gameState.entropyCount) * 0.01

        perspective =
            Mat4.makePerspective 45 (toFloat gameWidth / toFloat gameHeight) 0.01 1000

        camera =
            Mat4.makeLookAt
                (vec3 (gameState.space.width / 2) (gameState.space.height / 2) 800)
                (vec3 (gameState.space.width / 2) (gameState.space.height / 2) 0)
                (vec3 0 -1 0)

        entities =
            gameState.bodies
                |> Table.toList
                |> List.filterMap (renderBody gameState perspective camera time)
                |> List.concat

        healthBars =
            [ ship1 |> Maybe.map (renderHealthBar3D gameState perspective camera { x = 10, y = (toFloat gameHeight / 2) - 25 }) |> Maybe.withDefault []
            , ship2 |> Maybe.map (renderHealthBar3D gameState perspective camera { x = toFloat gameWidth - 54, y = (toFloat gameHeight / 2) - 25 }) |> Maybe.withDefault []
            ]
                |> List.concat
    in
    WebGL.toHtmlWith
        [ WebGL.clearColor 0.02 0.02 0.08 1.0
        , WebGL.alpha True
        , WebGL.depth 1
        , WebGL.antialias
        ]
        [ width gameWidth
        , height gameHeight
        , style "display" "block"
        , style "background" "radial-gradient(ellipse at center, #0a0a2e 0%, #000000 100%)"
        ]
        (renderSimpleStarfield gameState perspective camera ++ entities ++ healthBars)


renderSimpleStarfield : GameState -> Mat4 -> Mat4 -> List Entity
renderSimpleStarfield gameState perspective camera =
    let
        starPositions =
            List.range 0 50
                |> List.map (\i ->
                    let
                        x = toFloat (modBy 1200 (i * 137))
                        y = toFloat (modBy 700 (i * 223))
                        z = toFloat (modBy 200 (i * 31)) - 100
                    in
                    { position = vec3 x y z, normal = vec3 0 0 1, color = vec3 1 1 1 }
                )

        model = Mat4.identity

        uniforms =
            { perspective = perspective
            , camera = camera
            , model = model
            , uniformColor = vec3 1 1 1
            , opacity = 0.8
            }
    in
    [ WebGL.entityWith
        [ Blend.add Blend.srcAlpha Blend.one
        , DepthTest.always { write = False, near = 0, far = 1 }
        ]
        simpleVertexShader
        simpleFragmentShader
        (WebGL.points starPositions)
        uniforms
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


renderBody : GameState -> Mat4 -> Mat4 -> Float -> Body -> Maybe (List Entity)
renderBody gameState perspective camera time body =
    case body.bodyType of
        Planet _ ->
            Just (renderPlanet3D gameState perspective camera time body)

        Ship ship_ ->
            Just (renderShip3D gameState perspective camera time body ship_)

        Projectile _ ->
            Just (renderProjectile3D gameState perspective camera body)

        _ ->
            Nothing


renderPlanet3D : GameState -> Mat4 -> Mat4 -> Float -> Body -> List Entity
renderPlanet3D gameState perspective camera time planet =
    let
        model =
            Mat4.makeTranslate3 planet.position.x planet.position.y 0
                |> Mat4.scale3 planet.radius planet.radius planet.radius
                |> Mat4.rotate (time * 0.05) (vec3 0 0 1)

        uniforms =
            { perspective = perspective
            , camera = camera
            , model = model
            , lightDirection = Vec3.normalize (vec3 0.3 0.3 1)
            , ambientColor = vec3 0.3 0.2 0.1
            , diffuseColor = vec3 0.878 0.659 0.408
            , specularColor = vec3 0.9 0.8 0.7
            , shininess = 20
            , opacity = 1.0
            , time = time
            }
    in
    [ WebGL.entityWith
        [ DepthTest.default ]
        planetVertexShader
        planetFragmentShader
        (sphereMesh 1 32 24)  -- Higher resolution for smooth planet
        uniforms
    ]


renderShip3D : GameState -> Mat4 -> Mat4 -> Float -> Body -> { rotation : Float, rotationSpeed : Float, propulsion : PropulsionType, projectile : ProjectileType, maxCrew : Int, crew : Int } -> List Entity
renderShip3D gameState perspective camera time ship shipData =
    case shipData.propulsion of
        LittleGrayMenTech _ ->
            renderSaucerShip3D gameState perspective camera time ship shipData

        _ ->
            renderHumanShip3D gameState perspective camera time ship shipData


renderSaucerShip3D : GameState -> Mat4 -> Mat4 -> Float -> Body -> { rotation : Float, rotationSpeed : Float, propulsion : PropulsionType, projectile : ProjectileType, maxCrew : Int, crew : Int } -> List Entity
renderSaucerShip3D gameState perspective camera time ship shipData =
    let
        model =
            Mat4.makeTranslate3 ship.position.x ship.position.y 5
                |> Mat4.rotate (shipData.rotation) (vec3 0 0 1)
                |> Mat4.scale3 1.2 1.2 1.2  -- Slightly bigger for better visibility

        saucerUniforms =
            { perspective = perspective
            , camera = camera
            , model = model
            , lightDirection = Vec3.normalize (vec3 0.2 0.2 1)
            , ambientColor = vec3 0.4 0.4 0.45
            , diffuseColor = vec3 0.85 0.85 0.9
            , specularColor = vec3 1 1 1
            , shininess = 120  -- High shininess for metallic look
            , opacity = 1.0
            , time = time
            }

        -- Simple indicator lights
        frontLight =
            let
                lightX = ship.position.x + cos shipData.rotation * 30
                lightY = ship.position.y + sin shipData.rotation * 30
            in
            WebGL.entityWith
                [ Blend.add Blend.one Blend.one
                , DepthTest.default ]
                simpleVertexShader
                simpleFragmentShader
                (circleMesh 1 8)
                { perspective = perspective
                , camera = camera
                , model = Mat4.makeTranslate3 lightX lightY 7 |> Mat4.scale3 3 3 1
                , uniformColor = vec3 1 0.2 0.2  -- Red front light
                , opacity = 0.9
                }
    in
    [ WebGL.entityWith
        [ DepthTest.default
        , WebGL.Settings.cullFace WebGL.Settings.back  -- Cull back faces for cleaner rendering
        ]
        vertexShader
        fragmentShader
        (saucerBodyMesh 48)  -- Even higher segments for maximum smoothness
        saucerUniforms
    , frontLight
    ]


renderHumanShip3D : GameState -> Mat4 -> Mat4 -> Float -> Body -> { rotation : Float, rotationSpeed : Float, propulsion : PropulsionType, projectile : ProjectileType, maxCrew : Int, crew : Int } -> List Entity
renderHumanShip3D gameState perspective camera time ship shipData =
    let
        -- Check if ship is thrusting (W or Up arrow pressed)
        isThrusting = 
            Set.member "w" gameState.depressedKeys || 
            Set.member "W" gameState.depressedKeys ||
            Set.member "ArrowUp" gameState.depressedKeys

        -- Calculate thrust scale based on time (grows larger when held)
        thrustScale = 
            if isThrusting then
                10 + 15 * (0.5 + 0.5 * sin (time * 10))  -- Pulsing between 10 and 25
            else
                0

        model =
            Mat4.makeTranslate3 ship.position.x ship.position.y 0
                |> Mat4.rotate shipData.rotation (vec3 0 0 1)

        -- Metallic blue-green silver color
        bodyColor = vec3 0.5 0.6 0.7
        wingColor = vec3 0.4 0.5 0.6

        shipUniforms =
            { perspective = perspective
            , camera = camera
            , model = model
            , lightDirection = Vec3.normalize (vec3 0.3 0.3 1)
            , ambientColor = vec3 0.2 0.25 0.3
            , diffuseColor = bodyColor
            , specularColor = vec3 0.9 0.95 1.0  -- High specular for metallic look
            , shininess = 80  -- High shininess for metallic
            , opacity = 1.0
            , time = time
            }

        wingUniforms =
            { perspective = perspective
            , camera = camera
            , model = model
            , lightDirection = Vec3.normalize (vec3 0.3 0.3 1)
            , ambientColor = vec3 0.15 0.2 0.25
            , diffuseColor = wingColor
            , specularColor = vec3 0.8 0.85 0.9
            , shininess = 60
            , opacity = 1.0
            , time = time
            }

        cockpitModel =
            Mat4.makeTranslate3 
                (ship.position.x + cos shipData.rotation * 10) 
                (ship.position.y + sin shipData.rotation * 10) 
                8
                |> Mat4.rotate shipData.rotation (vec3 0 0 1)

        cockpitUniforms =
            { perspective = perspective
            , camera = camera
            , model = cockpitModel
            , lightDirection = Vec3.normalize (vec3 0.3 0.3 1)
            , ambientColor = vec3 0.2 0.3 0.4
            , diffuseColor = vec3 0.482 0.718 0.878
            , specularColor = vec3 1 1 1
            , shininess = 100
            , opacity = 0.9
            , time = time
            }

        -- Simple efficient thruster glow
        engineGlow =
            if thrustScale > 0 then
                let
                    -- Position at the back of the ship
                    engineX = ship.position.x - cos shipData.rotation * 35
                    engineY = ship.position.y - sin shipData.rotation * 35
                    
                    glowModel =
                        Mat4.makeTranslate3 engineX engineY 0
                            |> Mat4.scale3 (15 + thrustScale) (15 + thrustScale) 1
                in
                [ WebGL.entityWith
                    [ Blend.add Blend.one Blend.one
                    , DepthTest.default
                    ]
                    simpleVertexShader
                    simpleFragmentShader
                    (circleMesh 1 8)  -- Simple low-poly circle
                    { perspective = perspective
                    , camera = camera
                    , model = glowModel
                    , uniformColor = vec3 0.5 0.8 1.0
                    , opacity = 0.8
                    }
                ]
            else
                []

        -- Simple shield circle
        shieldModel =
            Mat4.makeTranslate3 ship.position.x ship.position.y 0
                |> Mat4.scale3 ship.radius ship.radius 1

        shieldUniforms =
            { perspective = perspective
            , camera = camera
            , model = shieldModel
            , uniformColor = vec3 0 0.8 1
            , opacity = 0.2
            }
    in
    [ WebGL.entityWith
        [ DepthTest.default ]
        vertexShader
        fragmentShader
        humanShipBodyMesh
        shipUniforms
    , WebGL.entityWith
        [ DepthTest.default ]
        vertexShader
        fragmentShader
        humanShipWingsMesh
        wingUniforms
    , WebGL.entityWith
        [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
        , DepthTest.default
        ]
        vertexShader
        fragmentShader
        (sphereMesh 8 16 12)  -- Higher res cockpit
        cockpitUniforms
    , WebGL.entityWith
        [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
        , DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
        ]
        simpleVertexShader
        simpleFragmentShader
        (circleMesh ship.radius 32)  -- Higher res shield
        shieldUniforms
    ] ++ engineGlow


renderProjectile3D : GameState -> Mat4 -> Mat4 -> Body -> List Entity
renderProjectile3D gameState perspective camera projectile =
    let
        model =
            Mat4.makeTranslate3 projectile.position.x projectile.position.y 5
                |> Mat4.scale3 2 2 2

        uniforms =
            { perspective = perspective
            , camera = camera
            , model = model
            , uniformColor = vec3 1 1 0
            , opacity = 1.0
            }
    in
    [ WebGL.entityWith
        [ DepthTest.default ]
        simpleVertexShader
        simpleFragmentShader
        (sphereMesh 1 8 6)
        uniforms
    ]


renderHealthBar3D : GameState -> Mat4 -> Mat4 -> Vector2D -> Body -> List Entity
renderHealthBar3D gameState perspective camera position ship =
    case ship.bodyType of
        Ship shipData ->
            let
                blockWidth = 20
                blockHeight = 5
                blockDepth = 3
                leftColumnCount = shipData.crew // 2
                rightColumnCount = shipData.crew - leftColumnCount

                makeBlock xOffset yOffset =
                    let
                        model =
                            Mat4.makeTranslate3 (position.x + xOffset) (position.y + yOffset) 400
                                |> Mat4.scale3 blockWidth blockHeight blockDepth

                        uniforms =
                            { perspective = perspective
                            , camera = camera
                            , model = model
                            , uniformColor = vec3 0 1 0
                            , opacity = 1.0
                            }
                    in
                    WebGL.entityWith
                        [ DepthTest.default ]
                        simpleVertexShader
                        simpleFragmentShader
                        boxMesh
                        uniforms

                leftBlocks =
                    List.range 0 (leftColumnCount - 1)
                        |> List.map (\i -> makeBlock 0 (toFloat i * (blockHeight + 2)))

                rightBlocks =
                    List.range 0 (rightColumnCount - 1)
                        |> List.map (\i -> makeBlock (blockWidth + 4) (toFloat i * (blockHeight + 2)))
            in
            leftBlocks ++ rightBlocks

        _ ->
            []


hashToColorVec3 : Int -> Vec3
hashToColorVec3 id =
    let
        hash =
            id
                |> String.fromInt
                |> sha256
                |> String.left 6

        hexToFloat hex =
            case String.toInt ("0x" ++ hex) of
                Just val ->
                    toFloat val / 255

                Nothing ->
                    0

        r = hexToFloat (String.slice 0 2 hash)
        g = hexToFloat (String.slice 2 4 hash)
        b = hexToFloat (String.slice 4 6 hash)
    in
    vec3 r g b


circleMesh : Float -> Int -> Mesh Vertex
circleMesh radius segments =
    let
        center =
            { position = vec3 0 0 0, normal = vec3 0 0 1, color = vec3 0.8 0.8 0.8 }

        vertices =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    let
                        angle = 2 * pi * toFloat i / toFloat segments
                        x = radius * cos angle
                        y = radius * sin angle
                    in
                    { position = vec3 x y 0, normal = vec3 0 0 1, color = vec3 0.8 0.8 0.8 }
                )

        triangles =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    ( center
                    , Maybe.withDefault center (List.head (List.drop i vertices))
                    , Maybe.withDefault center (List.head (List.drop ((i + 1) |> modBy segments) vertices))
                    )
                )
    in
    WebGL.triangles triangles


sphereMesh : Float -> Int -> Int -> Mesh Vertex
sphereMesh radius latitudes longitudes =
    let
        vertex lat lon =
            let
                theta = pi * toFloat lat / toFloat latitudes
                phi = 2 * pi * toFloat lon / toFloat longitudes
                x = radius * sin theta * cos phi
                y = radius * sin theta * sin phi
                z = radius * cos theta
                pos = vec3 x y z
            in
            { position = pos
            , normal = Vec3.normalize pos
            , color = vec3 0.8 0.8 0.8
            }

        vertices =
            List.range 0 latitudes
                |> List.concatMap (\lat ->
                    List.range 0 longitudes
                        |> List.map (vertex lat)
                )

        getVertex lat lon =
            vertices
                |> List.drop (lat * (longitudes + 1) + lon)
                |> List.head
                |> Maybe.withDefault { position = vec3 0 0 0, normal = vec3 0 1 0, color = vec3 0.8 0.8 0.8 }

        triangles =
            List.range 0 (latitudes - 1)
                |> List.concatMap (\lat ->
                    List.range 0 (longitudes - 1)
                        |> List.concatMap (\lon ->
                            [ ( getVertex lat lon
                              , getVertex (lat + 1) lon
                              , getVertex lat (lon + 1)
                              )
                            , ( getVertex (lat + 1) lon
                              , getVertex (lat + 1) (lon + 1)
                              , getVertex lat (lon + 1)
                              )
                            ]
                        )
                )
    in
    WebGL.triangles triangles


boxMesh : Mesh Vertex
boxMesh =
    let
        v0 = { position = vec3 -0.5 -0.5 -0.5, normal = vec3 0 0 -1, color = vec3 0.8 0.8 0.8 }
        v1 = { position = vec3 0.5 -0.5 -0.5, normal = vec3 0 0 -1, color = vec3 0.8 0.8 0.8 }
        v2 = { position = vec3 0.5 0.5 -0.5, normal = vec3 0 0 -1, color = vec3 0.8 0.8 0.8 }
        v3 = { position = vec3 -0.5 0.5 -0.5, normal = vec3 0 0 -1, color = vec3 0.8 0.8 0.8 }
        v4 = { position = vec3 -0.5 -0.5 0.5, normal = vec3 0 0 1, color = vec3 0.8 0.8 0.8 }
        v5 = { position = vec3 0.5 -0.5 0.5, normal = vec3 0 0 1, color = vec3 0.8 0.8 0.8 }
        v6 = { position = vec3 0.5 0.5 0.5, normal = vec3 0 0 1, color = vec3 0.8 0.8 0.8 }
        v7 = { position = vec3 -0.5 0.5 0.5, normal = vec3 0 0 1, color = vec3 0.8 0.8 0.8 }
    in
    WebGL.triangles
        [ ( v0, v1, v2 ), ( v0, v2, v3 )
        , ( v4, v6, v5 ), ( v4, v7, v6 )
        , ( v0, v4, v1 ), ( v1, v4, v5 )
        , ( v2, v6, v7 ), ( v2, v7, v3 )
        , ( v0, v3, v7 ), ( v0, v7, v4 )
        , ( v1, v5, v6 ), ( v1, v6, v2 )
        ]


saucerBodyMesh : Int -> Mesh Vertex
saucerBodyMesh segments =
    -- Clean, crisp UFO design
    let
        -- Metallic silver colors
        silverColor = vec3 0.8 0.82 0.85
        darkSilver = vec3 0.5 0.52 0.55
        brightSilver = vec3 0.9 0.92 0.95
        
        -- Simple two-part design: disc and dome
        discRadius = 40
        domeRadius = 20
        
        angleStep = 2 * pi / toFloat segments
        
        -- Create vertices for main disc body
        -- Top edge of disc
        discTopEdge =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    let
                        angle = toFloat i * angleStep
                        x = discRadius * cos angle
                        y = discRadius * sin angle
                        -- Slightly curved edge for smoother look
                        z = 2 - (discRadius - sqrt(x*x + y*y)) * 0.05
                    in
                    { position = vec3 x y z
                    , normal = vec3 (x/discRadius) (y/discRadius) 0.3 |> Vec3.normalize
                    , color = silverColor
                    }
                )
        
        -- Bottom edge of disc
        discBottomEdge =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    let
                        angle = toFloat i * angleStep
                        x = discRadius * cos angle
                        y = discRadius * sin angle
                        z = -2 + (discRadius - sqrt(x*x + y*y)) * 0.05
                    in
                    { position = vec3 x y z
                    , normal = vec3 (x/discRadius) (y/discRadius) (-0.3) |> Vec3.normalize
                    , color = darkSilver
                    }
                )
        
        -- Dome base (where dome meets disc)
        domeBaseEdge =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    let
                        angle = toFloat i * angleStep
                        x = domeRadius * cos angle
                        y = domeRadius * sin angle
                    in
                    { position = vec3 x y 2
                    , normal = vec3 (x/domeRadius) (y/domeRadius) 0.5 |> Vec3.normalize
                    , color = silverColor
                    }
                )
        
        -- Key points
        discCenter = { position = vec3 0 0 0, normal = vec3 0 0 1, color = silverColor }
        discBottom = { position = vec3 0 0 -3, normal = vec3 0 0 -1, color = darkSilver }
        domeTop = { position = vec3 0 0 12, normal = vec3 0 0 1, color = brightSilver }
        
        -- Build clean triangles
        -- Disc rim (smooth band around edge)
        discRim =
            List.range 0 (segments - 1)
                |> List.concatMap (\i ->
                    let
                        next = modBy segments (i + 1)
                        top1 = Maybe.withDefault discCenter (List.head (List.drop i discTopEdge))
                        top2 = Maybe.withDefault discCenter (List.head (List.drop next discTopEdge))
                        bot1 = Maybe.withDefault discBottom (List.head (List.drop i discBottomEdge))
                        bot2 = Maybe.withDefault discBottom (List.head (List.drop next discBottomEdge))
                    in
                    [ (top1, bot1, bot2)
                    , (top1, bot2, top2)
                    ]
                )
        
        -- Disc bottom (flat bottom)
        discBottomFace =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    let
                        next = modBy segments (i + 1)
                        v1 = Maybe.withDefault discBottom (List.head (List.drop i discBottomEdge))
                        v2 = Maybe.withDefault discBottom (List.head (List.drop next discBottomEdge))
                    in
                    (discBottom, v2, v1)
                )
        
        -- Disc top surface (from edge to dome base)
        discTopSurface =
            List.range 0 (segments - 1)
                |> List.concatMap (\i ->
                    let
                        next = modBy segments (i + 1)
                        outer1 = Maybe.withDefault discCenter (List.head (List.drop i discTopEdge))
                        outer2 = Maybe.withDefault discCenter (List.head (List.drop next discTopEdge))
                        inner1 = Maybe.withDefault discCenter (List.head (List.drop i domeBaseEdge))
                        inner2 = Maybe.withDefault discCenter (List.head (List.drop next domeBaseEdge))
                    in
                    [ (outer1, inner1, inner2)
                    , (outer1, inner2, outer2)
                    ]
                )
        
        -- Dome (smooth hemisphere)
        dome =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    let
                        next = modBy segments (i + 1)
                        v1 = Maybe.withDefault domeTop (List.head (List.drop i domeBaseEdge))
                        v2 = Maybe.withDefault domeTop (List.head (List.drop next domeBaseEdge))
                    in
                    (domeTop, v1, v2)
                )
    in
    WebGL.triangles
        ( discRim 
          ++ discBottomFace 
          ++ discTopSurface 
          ++ dome
        )


humanShipBodyMesh : Mesh Vertex
humanShipBodyMesh =
    -- Premium X-Wing inspired fighter design
    let
        -- Main fuselage colors
        bodyColor = vec3 0.85 0.85 0.9
        darkColor = vec3 0.3 0.35 0.4
        accentColor = vec3 0.9 0.1 0.1  -- Red accents
        
        -- Fuselage (elongated body)
        nose = { position = vec3 40 0 3, normal = vec3 1 0 0, color = bodyColor }
        noseTop = { position = vec3 35 0 8, normal = vec3 0.8 0 0.5, color = bodyColor }
        
        -- Cockpit area
        cockpitFront = { position = vec3 20 0 12, normal = vec3 0.3 0 0.8, color = darkColor }
        cockpitBack = { position = vec3 5 0 10, normal = vec3 0 0 1, color = darkColor }
        
        -- Mid body
        midLeft = { position = vec3 0 -8 5, normal = vec3 0 -1 0, color = bodyColor }
        midRight = { position = vec3 0 8 5, normal = vec3 0 1 0, color = bodyColor }
        midBottom = { position = vec3 0 0 0, normal = vec3 0 0 -1, color = darkColor }
        
        -- Rear section
        rearLeft = { position = vec3 -30 -6 4, normal = vec3 -0.5 -0.5 0, color = bodyColor }
        rearRight = { position = vec3 -30 6 4, normal = vec3 -0.5 0.5 0, color = bodyColor }
        rearTop = { position = vec3 -30 0 8, normal = vec3 -1 0 0.5, color = bodyColor }
        rearBottom = { position = vec3 -30 0 0, normal = vec3 -1 0 -0.5, color = darkColor }
        
        -- Engine intakes
        intakeLeft = { position = vec3 -20 -10 3, normal = vec3 0 -1 0, color = darkColor }
        intakeRight = { position = vec3 -20 10 3, normal = vec3 0 1 0, color = darkColor }
    in
    WebGL.triangles
        [ -- Nose cone
          ( nose, noseTop, cockpitFront )
        , ( nose, cockpitFront, midLeft )
        , ( nose, midRight, cockpitFront )
        , ( nose, midLeft, midBottom )
        , ( nose, midBottom, midRight )
        
          -- Cockpit canopy
        , ( cockpitFront, noseTop, cockpitBack )
        , ( cockpitFront, cockpitBack, midLeft )
        , ( cockpitFront, midRight, cockpitBack )
        
          -- Main body
        , ( midLeft, cockpitBack, rearTop )
        , ( midRight, rearTop, cockpitBack )
        , ( midLeft, rearTop, rearLeft )
        , ( midRight, rearRight, rearTop )
        , ( midBottom, rearLeft, rearBottom )
        , ( midBottom, rearBottom, rearRight )
        , ( midLeft, rearLeft, midBottom )
        , ( midRight, midBottom, rearRight )
        
          -- Engine intakes
        , ( intakeLeft, rearLeft, midLeft )
        , ( intakeRight, midRight, rearRight )
        
          -- Rear face
        , ( rearTop, rearLeft, rearBottom )
        , ( rearTop, rearBottom, rearRight )
        ]


humanShipWingsMesh : Mesh Vertex
humanShipWingsMesh =
    -- X-Wing style swept wings with weapon pods
    let
        wingColor = vec3 0.8 0.8 0.85
        darkColor = vec3 0.25 0.25 0.3
        
        -- Left wing (S-foil configuration)
        leftRoot = { position = vec3 0 -12 3, normal = vec3 0 -1 0.2, color = wingColor }
        leftMid = { position = vec3 -15 -28 2, normal = vec3 0 -1 0.1, color = wingColor }
        leftTip = { position = vec3 -30 -38 1, normal = vec3 -0.3 -1 0, color = wingColor }
        leftBack = { position = vec3 -25 -18 2, normal = vec3 -0.5 -0.5 0, color = wingColor }
        leftWeapon = { position = vec3 -12 -32 3, normal = vec3 0 0 1, color = darkColor }
        
        -- Right wing (S-foil configuration)
        rightRoot = { position = vec3 0 12 3, normal = vec3 0 1 0.2, color = wingColor }
        rightMid = { position = vec3 -15 28 2, normal = vec3 0 1 0.1, color = wingColor }
        rightTip = { position = vec3 -30 38 1, normal = vec3 -0.3 1 0, color = wingColor }
        rightBack = { position = vec3 -25 18 2, normal = vec3 -0.5 0.5 0, color = wingColor }
        rightWeapon = { position = vec3 -12 32 3, normal = vec3 0 0 1, color = darkColor }
    in
    WebGL.triangles
        [ -- Left wing
          ( leftRoot, leftMid, leftBack )
        , ( leftMid, leftTip, leftBack )
        , ( leftMid, leftWeapon, leftTip )
        
          -- Right wing
        , ( rightRoot, rightBack, rightMid )
        , ( rightMid, rightBack, rightTip )
        , ( rightMid, rightTip, rightWeapon )
        ]


coneMesh : Float -> Float -> Int -> Mesh Vertex
coneMesh radius height segments =
    let
        tip =
            { position = vec3 0 0 height
            , normal = vec3 0 0 1
            , color = vec3 1 0.5 0
            }

        baseVertices =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    let
                        angle = 2 * pi * toFloat i / toFloat segments
                        x = radius * cos angle
                        y = radius * sin angle
                    in
                    { position = vec3 x y 0
                    , normal = Vec3.normalize (vec3 x y 0.5)
                    , color = vec3 1 0.3 0
                    }
                )

        triangles =
            List.range 0 (segments - 1)
                |> List.map (\i ->
                    ( tip
                    , Maybe.withDefault tip (List.head (List.drop i baseVertices))
                    , Maybe.withDefault tip (List.head (List.drop ((i + 1) |> modBy segments) baseVertices))
                    )
                )
    in
    WebGL.triangles triangles


vertexShader : Shader Vertex Uniforms { vPosition : Vec3, vNormal : Vec3, vColor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 model;
        varying vec3 vPosition;
        varying vec3 vNormal;
        varying vec3 vColor;

        void main () {
            vec4 worldPosition = model * vec4(position, 1.0);
            vPosition = worldPosition.xyz;
            vNormal = normalize((model * vec4(normal, 0.0)).xyz);
            vColor = color;
            gl_Position = perspective * camera * worldPosition;
        }
    |]


fragmentShader : Shader {} Uniforms { vPosition : Vec3, vNormal : Vec3, vColor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightDirection;
        uniform vec3 ambientColor;
        uniform vec3 diffuseColor;
        uniform vec3 specularColor;
        uniform float shininess;
        uniform float opacity;
        varying vec3 vPosition;
        varying vec3 vNormal;
        varying vec3 vColor;

        void main () {
            vec3 normal = normalize(vNormal);
            vec3 lightDir = normalize(lightDirection);
            
            float diff = max(dot(normal, lightDir), 0.0);
            vec3 diffuse = diff * diffuseColor * vColor;
            
            vec3 viewDir = normalize(-vPosition);
            vec3 reflectDir = reflect(-lightDir, normal);
            float spec = pow(max(dot(viewDir, reflectDir), 0.0), shininess);
            vec3 specular = spec * specularColor;
            
            vec3 result = ambientColor * vColor + diffuse + specular;
            gl_FragColor = vec4(result, opacity);
        }
    |]


simpleVertexShader : Shader Vertex SimpleUniforms { vColor : Vec3 }
simpleVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 model;
        uniform vec3 uniformColor;
        varying vec3 vColor;

        void main () {
            vec4 worldPosition = model * vec4(position, 1.0);
            vColor = uniformColor * color;
            gl_Position = perspective * camera * worldPosition;
            gl_PointSize = 2.0;
        }
    |]


simpleFragmentShader : Shader {} SimpleUniforms { vColor : Vec3 }
simpleFragmentShader =
    [glsl|
        precision mediump float;
        uniform float opacity;
        varying vec3 vColor;

        void main () {
            gl_FragColor = vec4(vColor, opacity);
        }
    |]


planetVertexShader : Shader Vertex Uniforms { vPosition : Vec3, vNormal : Vec3, vColor : Vec3, vTexCoord : Vec2 }
planetVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 model;
        uniform float time;
        varying vec3 vPosition;
        varying vec3 vNormal;
        varying vec3 vColor;
        varying vec2 vTexCoord;

        void main () {
            vec4 worldPosition = model * vec4(position, 1.0);
            vPosition = worldPosition.xyz;
            vNormal = normalize((model * vec4(normal, 0.0)).xyz);
            vColor = color;
            
            float theta = atan(position.y, position.x);
            float phi = acos(position.z / length(position));
            vTexCoord = vec2(theta / (2.0 * 3.14159) + 0.5, phi / 3.14159);
            
            gl_Position = perspective * camera * worldPosition;
        }
    |]


planetFragmentShader : Shader {} Uniforms { vPosition : Vec3, vNormal : Vec3, vColor : Vec3, vTexCoord : Vec2 }
planetFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightDirection;
        uniform vec3 ambientColor;
        uniform vec3 diffuseColor;
        uniform vec3 specularColor;
        uniform float shininess;
        uniform float opacity;
        uniform float time;
        varying vec3 vPosition;
        varying vec3 vNormal;
        varying vec3 vColor;
        varying vec2 vTexCoord;

        float noise(vec2 p) {
            return sin(p.x * 10.0) * sin(p.y * 10.0);
        }

        void main () {
            vec3 normal = normalize(vNormal);
            vec3 lightDir = normalize(lightDirection);
            
            // Jupiter-like bands
            float bandFreq = 12.0;
            float bands = sin(vTexCoord.y * bandFreq + noise(vTexCoord * 3.0) * 0.3);
            float bands2 = sin(vTexCoord.y * bandFreq * 1.5 + 2.0 + noise(vTexCoord * 4.0) * 0.2);
            
            vec3 baseColor = vec3(0.878, 0.659, 0.408);  // Base Jupiter color
            vec3 darkBand = vec3(0.757, 0.498, 0.243);   // Darker band
            vec3 lightBand = vec3(0.937, 0.812, 0.569);  // Lighter band
            
            vec3 bandColor = baseColor;
            if (bands > 0.0) {
                bandColor = mix(baseColor, darkBand, bands);
            } else {
                bandColor = mix(baseColor, lightBand, -bands * 0.5);
            }
            
            if (bands2 > 0.3) {
                bandColor = mix(bandColor, vec3(0.651, 0.416, 0.157), (bands2 - 0.3) * 0.5);
            }
            
            // Great Red Spot
            vec2 spotCenter = vec2(0.3 + time * 0.02, 0.6);  // Slowly rotating
            float spotDist = distance(vTexCoord, spotCenter);
            if (spotDist < 0.12) {
                float spotIntensity = 1.0 - spotDist / 0.12;
                vec3 spotColor = vec3(0.757, 0.267, 0.055);
                bandColor = mix(bandColor, spotColor, spotIntensity * 0.8);
            }
            
            // Smaller storm
            vec2 storm2 = vec2(0.7 + time * 0.015, 0.4);
            float storm2Dist = distance(vTexCoord, storm2);
            if (storm2Dist < 0.06) {
                float stormIntensity = 1.0 - storm2Dist / 0.06;
                vec3 stormColor = vec3(0.878, 0.522, 0.322);
                bandColor = mix(bandColor, stormColor, stormIntensity * 0.6);
            }
            
            float diff = max(dot(normal, lightDir), 0.0);
            vec3 diffuse = diff * bandColor;
            
            vec3 viewDir = normalize(-vPosition);
            vec3 reflectDir = reflect(-lightDir, normal);
            float spec = pow(max(dot(viewDir, reflectDir), 0.0), shininess);
            vec3 specular = spec * specularColor * 0.3;  // Subtle specular
            
            vec3 result = ambientColor + diffuse + specular;
            gl_FragColor = vec4(result, opacity);
        }
    |]


-- Model Viewer for inspecting 3D models
renderModelViewer : ModelViewerState -> Int -> Html msg
renderModelViewer viewerState time =
    let
        width = 800
        height = 600
        
        -- Auto rotation if enabled
        autoRotY = if viewerState.autoRotate then toFloat time * 0.01 else 0
        
        perspective =
            Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 1000
        
        camera =
            Mat4.makeLookAt
                (vec3 0 0 viewerState.zoom)
                (vec3 0 0 0)
                (vec3 0 1 0)
        
        -- Combined rotation
        model =
            Mat4.identity
                |> Mat4.rotate (degrees viewerState.rotationX) (vec3 1 0 0)
                |> Mat4.rotate (degrees (viewerState.rotationY + autoRotY)) (vec3 0 1 0)
                |> Mat4.rotate (degrees viewerState.rotationZ) (vec3 0 0 1)
        
        -- Standard uniforms for the object
        baseColor = 
            if viewerState.useCustomColor then
                vec3 viewerState.colorR viewerState.colorG viewerState.colorB
            else
                case viewerState.selectedObject of
                    ModelHumanShip -> vec3 0.5 0.6 0.7  -- Metallic blue like in game
                    ModelSaucer -> vec3 0.85 0.85 0.9   -- Silver
                    ModelPlanet -> vec3 0.8 0.6 0.4      -- Jupiter-like
                    ModelProjectile -> vec3 1 0.5 0      -- Orange
        
        uniforms =
            { perspective = perspective
            , camera = camera
            , model = model
            , lightDirection = Vec3.normalize (vec3 0.3 0.3 1)
            , ambientColor = Vec3.scale 0.3 baseColor
            , diffuseColor = baseColor
            , specularColor = vec3 1 1 1
            , shininess = 80
            , opacity = 1.0
            , time = toFloat time * 0.01
            }
        
        -- Select mesh based on selected object
        meshes =
            case viewerState.selectedObject of
                ModelHumanShip ->
                    [ (humanShipBodyMesh, uniforms)
                    , (humanShipWingsMesh, uniforms)
                    ]
                
                ModelSaucer ->
                    [ (saucerBodyMesh 48, uniforms) ]
                
                ModelPlanet ->
                    [ (sphereMesh 50 32 24, uniforms) ]
                
                ModelProjectile ->
                    [ (sphereMesh 5 16 12, uniforms) ]
        
        -- Render settings (wireframe not available in WebGL)
        renderSettings =
            [ DepthTest.default
            ]
        
        entities =
            meshes
                |> List.map (\(mesh, uni) ->
                    WebGL.entityWith
                        renderSettings
                        vertexShader
                        fragmentShader
                        mesh
                        uni
                )
    in
    WebGL.toHtmlWith
        [ WebGL.clearColor 0.1 0.1 0.15 1.0
        , WebGL.alpha True
        , WebGL.depth 1
        , WebGL.antialias
        ]
        [ Html.Attributes.width width
        , Html.Attributes.height height
        , style "display" "block"
        , style "background" "linear-gradient(135deg, #1a1a2e 0%, #0f0f1e 100%)"
        , style "border" "2px solid #333"
        , style "border-radius" "8px"
        ]
        entities