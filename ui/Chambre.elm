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
    , floor1Walls : Drawable T.Vertex
    , floor1WallTile : Maybe Texture
    , farWallDoor : Drawable T.Vertex
    , farWallDoorTile : Maybe Texture
    , floor1Windows : Drawable T.Vertex
    , floor1WindowTile : Maybe Texture
    }


make : Vec3 -> Vec3 -> Chambre
make coord scale =
    { coord = coord
    , scale = scale
    , floor = makeFloor
    , floorTile = Nothing
    , floor1Walls = makeFloor1Walls
    , floor1WallTile = Nothing
    , farWallDoor = makeFarWallDoor
    , farWallDoorTile = Nothing
    , floor1Windows = makeFloor1Windows
    , floor1WindowTile = Nothing
    }


setChambreTiles : Texture -> Texture -> Texture -> Texture -> Chambre -> Chambre
setChambreTiles floorTile floor1WallTile farWallDoorTile floor1WindowTile chambre =
    { chambre
        | floorTile = Just floorTile
        , floor1WallTile = Just floor1WallTile
        , farWallDoorTile = Just farWallDoorTile
        , floor1WindowTile = Just floor1WindowTile
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
                chambre.floor1WallTile
                chambre.farWallDoorTile
                chambre.floor1WindowTile
        of
            Just ( floorTile, floor1WallTile, farWallDoorTile, windowTile ) ->
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
                    chambre.floor1Walls
                    { perspective = perspective
                    , modelView = modelView_
                    , texture = floor1WallTile
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
                    chambre.floor1Windows
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


makeFloor1Walls : Drawable T.Vertex
makeFloor1Walls =
    Triangle <|
        concatMap T.makeFace
            [ --Far wall. Make room for two windows and one door
              ( vec3 -7 2 -7, vec3 -5 2 -7, vec3 -7 0 -7, vec3 -5 0 -7 )
            , ( vec3 -3 2 -7, vec3 -1 2 -7, vec3 -3 0 -7, vec3 -1 0 -7 )
            , ( vec3 1 2 -7, vec3 3 2 -7, vec3 1 0 -7, vec3 3 0 -7 )
            , ( vec3 5 2 -7, vec3 7 2 -7, vec3 5 0 -7, vec3 7 0 -7 )
              -- Right wall - three windows.
            , ( vec3 7 2 -7, vec3 7 2 -5, vec3 7 0 -7, vec3 7 0 -5 )
            , ( vec3 7 2 -3, vec3 7 2 -1, vec3 7 0 -3, vec3 7 0 -1 )
            , ( vec3 7 2 1, vec3 7 2 3, vec3 7 0 1, vec3 7 0 3 )
            , ( vec3 7 2 5, vec3 7 2 7, vec3 7 0 5, vec3 7 0 7 )
              -- Left wall - three windows.
            , ( vec3 -7 2 -5, vec3 -7 2 -7, vec3 -7 0 -5, vec3 -7 0 -7 )
            , ( vec3 -7 2 -1, vec3 -7 2 -3, vec3 -7 0 -1, vec3 -7 0 -3 )
            , ( vec3 -7 2 3, vec3 -7 2 1, vec3 -7 0 3, vec3 -7 0 1 )
            , ( vec3 -7 2 7, vec3 -7 2 5, vec3 -7 0 7, vec3 -7 0 5 )
            ]


makeFarWallDoor : Drawable T.Vertex
makeFarWallDoor =
    Triangle <|
        concatMap T.makeFace
            [ ( vec3 -1 2 -7, vec3 1 2 -7, vec3 -1 0 -7, vec3 1 0 -7 )
            ]


makeFloor1Windows : Drawable T.Vertex
makeFloor1Windows =
    Triangle <|
        concatMap T.makeFace
            [ -- Far wall - two windows
              ( vec3 -5 2 -7, vec3 -3 2 -7, vec3 -5 0 -7, vec3 -3 0 -7 )
            , ( vec3 3 2 -7, vec3 5 2 -7, vec3 3 0 -7, vec3 5 0 -7 )
              -- Right wall - three windows
            , ( vec3 7 2 -5, vec3 7 2 -3, vec3 7 0 -5, vec3 7 0 -3 )
            , ( vec3 7 2 -1, vec3 7 2 1, vec3 7 0 -1, vec3 7 0 1 )
            , ( vec3 7 2 3, vec3 7 2 5, vec3 7 0 3, vec3 7 0 5 )
              -- Right wall - three windows
            , ( vec3 -7 2 -3, vec3 -7 2 -5, vec3 -7 0 -3, vec3 -7 0 -5 )
            , ( vec3 -7 2 1, vec3 -7 2 -1, vec3 -7 0 1, vec3 -7 0 -1 )
            , ( vec3 -7 2 5, vec3 -7 2 3, vec3 -7 0 5, vec3 -7 0 3 )
            ]


modelView : Chambre -> Mat4
modelView chambre =
    mul (makeTranslate chambre.coord) (makeScale chambre.scale)
