module Chambre exposing (Chambre, make, setChambreTiles, view)

import Colored as C
import List exposing (concatMap)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (vec4)
import Math.Matrix4 exposing (Mat4, mul, makeTranslate, makeScale)
import Maybe as Maybe
import Textured as T
import WebGL exposing (Drawable(..), Renderable, Texture)


type alias Chambre =
    { coord : Vec3
    , scale : Vec3
    , floor : Drawable T.Vertex
    , floorTile : Maybe Texture
    , farWall : Drawable T.Vertex
    , stoneWallTile : Maybe Texture
    , farWallDoor : Drawable T.Vertex
    , farWallDoorTile : Maybe Texture
    , farWallWindows : Drawable T.Vertex
    , farWallWindowTile : Maybe Texture
    }


make : Vec3 -> Vec3 -> Chambre
make coord scale =
    { coord = coord
    , scale = scale
    , floor = makeFloor
    , floorTile = Nothing
    , farWall = makeFarWall
    , stoneWallTile = Nothing
    , farWallDoor = makeFarWallDoor
    , farWallDoorTile = Nothing
    , farWallWindows = makeFarWallWindows
    , farWallWindowTile = Nothing
    }


setChambreTiles : Texture -> Texture -> Texture -> Texture -> Chambre -> Chambre
setChambreTiles floorTile stoneWallTile farWallDoorTile windowTile chambre =
    { chambre
        | floorTile = Just floorTile
        , stoneWallTile = Just stoneWallTile
        , farWallDoorTile = Just farWallDoorTile
        , farWallWindowTile = Just windowTile
    }


view : Mat4 -> Chambre -> List Renderable
view perspective chambre =
    let
        modelView_ =
            modelView chambre
    in
        case
            Maybe.map4 (,,,)
                chambre.floorTile
                chambre.stoneWallTile
                chambre.farWallDoorTile
                chambre.farWallWindowTile
        of
            Just ( floorTile, stoneWallTile, farWallDoorTile, windowTile ) ->
                [ WebGL.render
                    T.vertexShader
                    T.fragmentShader
                    chambre.floor
                    { perspective = perspective
                    , modelView = modelView_
                    , texture = floorTile
                    }
                , WebGL.render
                    T.vertexShader
                    T.fragmentShader
                    chambre.farWall
                    { perspective = perspective
                    , modelView = modelView_
                    , texture = stoneWallTile
                    }
                , WebGL.render
                    T.vertexShader
                    T.fragmentShader
                    chambre.farWallDoor
                    { perspective = perspective
                    , modelView = modelView_
                    , texture = farWallDoorTile
                    }
                , WebGL.render
                    T.vertexShader
                    T.fragmentShader
                    chambre.farWallWindows
                    { perspective = perspective
                    , modelView = modelView_
                    , texture = windowTile
                    }
                ]

            Nothing ->
                []


makeFloor : Drawable T.Vertex
makeFloor =
    Triangle <|
        concatMap T.makeFace
            [ -- Row 1
              ( vec3 -7 0 -7, vec3 -5 0 -7, vec3 -7 0 -5, vec3 -5 0 -5 )
            , ( vec3 -5 0 -7, vec3 -3 0 -7, vec3 -5 0 -5, vec3 -3 0 -5 )
            , ( vec3 -3 0 -7, vec3 -1 0 -7, vec3 -3 0 -5, vec3 -1 0 -5 )
            , ( vec3 -1 0 -7, vec3 1 0 -7, vec3 -1 0 -5, vec3 1 0 -5 )
            , ( vec3 1 0 -7, vec3 3 0 -7, vec3 1 0 -5, vec3 3 0 -5 )
            , ( vec3 3 0 -7, vec3 5 0 -7, vec3 3 0 -5, vec3 5 0 -5 )
            , ( vec3 5 0 -7, vec3 7 0 -7, vec3 5 0 -5, vec3 7 0 -5 )
              -- Row 2
            , ( vec3 -7 0 -5, vec3 -5 0 -5, vec3 -7 0 -3, vec3 -5 0 -3 )
            , ( vec3 -5 0 -5, vec3 -3 0 -5, vec3 -5 0 -3, vec3 -3 0 -3 )
            , ( vec3 -3 0 -5, vec3 -1 0 -5, vec3 -3 0 -3, vec3 -1 0 -3 )
            , ( vec3 -1 0 -5, vec3 1 0 -5, vec3 -1 0 -3, vec3 1 0 -3 )
            , ( vec3 1 0 -5, vec3 3 0 -5, vec3 1 0 -3, vec3 3 0 -3 )
            , ( vec3 3 0 -5, vec3 5 0 -5, vec3 3 0 -3, vec3 5 0 -3 )
            , ( vec3 5 0 -5, vec3 7 0 -5, vec3 5 0 -3, vec3 7 0 -3 )
              -- Row 3
            , ( vec3 -7 0 -3, vec3 -5 0 -3, vec3 -7 0 -1, vec3 -5 0 -1 )
            , ( vec3 -5 0 -3, vec3 -3 0 -3, vec3 -5 0 -1, vec3 -3 0 -1 )
            , ( vec3 -3 0 -3, vec3 -1 0 -3, vec3 -3 0 -1, vec3 -1 0 -1 )
            , ( vec3 -1 0 -3, vec3 1 0 -3, vec3 -1 0 -1, vec3 1 0 -1 )
            , ( vec3 1 0 -3, vec3 3 0 -3, vec3 1 0 -1, vec3 3 0 -1 )
            , ( vec3 3 0 -3, vec3 5 0 -3, vec3 3 0 -1, vec3 5 0 -1 )
            , ( vec3 5 0 -3, vec3 7 0 -3, vec3 5 0 -1, vec3 7 0 -1 )
              -- Row 4
            , ( vec3 -7 0 -1, vec3 -5 0 -1, vec3 -7 0 1, vec3 -5 0 1 )
            , ( vec3 -5 0 -1, vec3 -3 0 -1, vec3 -5 0 1, vec3 -3 0 1 )
            , ( vec3 -3 0 -1, vec3 -1 0 -1, vec3 -3 0 1, vec3 -1 0 1 )
            , ( vec3 -1 0 -1, vec3 1 0 -1, vec3 -1 0 1, vec3 1 0 1 )
            , ( vec3 1 0 -1, vec3 3 0 -1, vec3 1 0 1, vec3 3 0 1 )
            , ( vec3 3 0 -1, vec3 5 0 -1, vec3 3 0 1, vec3 5 0 1 )
            , ( vec3 5 0 -1, vec3 7 0 -1, vec3 5 0 1, vec3 7 0 1 )
              -- Row 5
            , ( vec3 -7 0 1, vec3 -5 0 1, vec3 -7 0 3, vec3 -5 0 3 )
            , ( vec3 -5 0 1, vec3 -3 0 1, vec3 -5 0 3, vec3 -3 0 3 )
            , ( vec3 -3 0 1, vec3 -1 0 1, vec3 -3 0 3, vec3 -1 0 3 )
            , ( vec3 -1 0 1, vec3 1 0 1, vec3 -1 0 3, vec3 1 0 3 )
            , ( vec3 1 0 1, vec3 3 0 1, vec3 1 0 3, vec3 3 0 3 )
            , ( vec3 3 0 1, vec3 5 0 1, vec3 3 0 3, vec3 5 0 3 )
            , ( vec3 5 0 1, vec3 7 0 1, vec3 5 0 3, vec3 7 0 3 )
              -- Row 6
            , ( vec3 -7 0 3, vec3 -5 0 3, vec3 -7 0 5, vec3 -5 0 5 )
            , ( vec3 -5 0 3, vec3 -3 0 3, vec3 -5 0 5, vec3 -3 0 5 )
            , ( vec3 -3 0 3, vec3 -1 0 3, vec3 -3 0 5, vec3 -1 0 5 )
            , ( vec3 -1 0 3, vec3 1 0 3, vec3 -1 0 5, vec3 1 0 5 )
            , ( vec3 1 0 3, vec3 3 0 3, vec3 1 0 5, vec3 3 0 5 )
            , ( vec3 3 0 3, vec3 5 0 3, vec3 3 0 5, vec3 5 0 5 )
            , ( vec3 5 0 3, vec3 7 0 3, vec3 5 0 5, vec3 7 0 5 )
              -- Row 6
            , ( vec3 -7 0 5, vec3 -5 0 5, vec3 -7 0 7, vec3 -5 0 7 )
            , ( vec3 -5 0 5, vec3 -3 0 5, vec3 -5 0 7, vec3 -3 0 7 )
            , ( vec3 -3 0 5, vec3 -1 0 5, vec3 -3 0 7, vec3 -1 0 7 )
            , ( vec3 -1 0 5, vec3 1 0 5, vec3 -1 0 7, vec3 1 0 7 )
            , ( vec3 1 0 5, vec3 3 0 5, vec3 1 0 7, vec3 3 0 7 )
            , ( vec3 3 0 5, vec3 5 0 5, vec3 3 0 7, vec3 5 0 7 )
            , ( vec3 5 0 5, vec3 7 0 5, vec3 5 0 7, vec3 7 0 7 )
            ]


makeFarWall : Drawable T.Vertex
makeFarWall =
    Triangle <|
        concatMap T.makeFace
            [ -- Bottom row - give room for a door.
              ( vec3 -7 2 -7, vec3 -5 2 -7, vec3 -7 0 -7, vec3 -5 0 -7 )
              --, ( vec3 -5 2 -7, vec3 -3 2 -7, vec3 -5 0 -7, vec3 -3 0 -7 )
            , ( vec3 -3 2 -7, vec3 -1 2 -7, vec3 -3 0 -7, vec3 -1 0 -7 )
            , ( vec3 1 2 -7, vec3 3 2 -7, vec3 1 0 -7, vec3 3 0 -7 )
              --, ( vec3 3 2 -7, vec3 5 2 -7, vec3 3 0 -7, vec3 5 0 -7 )
            , ( vec3 5 2 -7, vec3 7 2 -7, vec3 5 0 -7, vec3 7 0 -7 )
            ]


makeFarWallDoor : Drawable T.Vertex
makeFarWallDoor =
    Triangle <|
        concatMap T.makeFace
            [ ( vec3 -1 2 -7, vec3 1 2 -7, vec3 -1 0 -7, vec3 1 0 -7 )
            ]


makeFarWallWindows : Drawable T.Vertex
makeFarWallWindows =
    Triangle <|
        concatMap T.makeFace
            [ ( vec3 -5 2 -7, vec3 -3 2 -7, vec3 -5 0 -7, vec3 -3 0 -7 )
            , ( vec3 3 2 -7, vec3 5 2 -7, vec3 3 0 -7, vec3 5 0 -7 )
            ]


modelView : Chambre -> Mat4
modelView chambre =
    mul (makeTranslate chambre.coord) (makeScale chambre.scale)
