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
    }


setChambreTiles : Texture -> Texture -> Texture -> Chambre -> Chambre
setChambreTiles floorTile stoneWallTile farWallDoorTile chambre =
    { chambre
        | floorTile = Just floorTile
        , stoneWallTile = Just stoneWallTile
        , farWallDoorTile = Just farWallDoorTile
    }


view : Mat4 -> Chambre -> List Renderable
view perspective chambre =
    let
        modelView_ =
            modelView chambre
    in
        case
            Maybe.map3 (,,)
                chambre.floorTile
                chambre.stoneWallTile
                chambre.farWallDoorTile
        of
            Just ( floorTile, stoneWallTile, farWallDoorTile ) ->
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
                ]

            Nothing ->
                []


makeFloor : Drawable T.Vertex
makeFloor =
    Triangle <|
        concatMap T.makeFace
            [ -- Row 1
              ( vec3 -6 0 -6, vec3 -4 0 -6, vec3 -6 0 -4, vec3 -4 0 -4 )
            , ( vec3 -4 0 -6, vec3 -2 0 -6, vec3 -4 0 -4, vec3 -2 0 -4 )
            , ( vec3 -2 0 -6, vec3 0 0 -6, vec3 -2 0 -4, vec3 0 0 -4 )
            , ( vec3 0 0 -6, vec3 2 0 -6, vec3 0 0 -4, vec3 2 0 -4 )
            , ( vec3 2 0 -6, vec3 4 0 -6, vec3 2 0 -4, vec3 4 0 -4 )
            , ( vec3 4 0 -6, vec3 6 0 -6, vec3 4 0 -4, vec3 6 0 -4 )
              -- Row 2
            , ( vec3 -6 0 -4, vec3 -4 0 -4, vec3 -6 0 -2, vec3 -4 0 -2 )
            , ( vec3 -4 0 -4, vec3 -2 0 -4, vec3 -4 0 -2, vec3 -2 0 -2 )
            , ( vec3 -2 0 -4, vec3 0 0 -4, vec3 -2 0 -2, vec3 0 0 -2 )
            , ( vec3 0 0 -4, vec3 2 0 -4, vec3 0 0 -2, vec3 2 0 -2 )
            , ( vec3 2 0 -4, vec3 4 0 -4, vec3 2 0 -2, vec3 4 0 -2 )
            , ( vec3 4 0 -4, vec3 6 0 -4, vec3 4 0 -2, vec3 6 0 -2 )
              -- Row 3
            , ( vec3 -6 0 -2, vec3 -4 0 -2, vec3 -6 0 0, vec3 -4 0 0 )
            , ( vec3 -4 0 -2, vec3 -2 0 -2, vec3 -4 0 0, vec3 -2 0 0 )
            , ( vec3 -2 0 -2, vec3 0 0 -2, vec3 -2 0 0, vec3 0 0 0 )
            , ( vec3 0 0 -2, vec3 2 0 -2, vec3 0 0 0, vec3 2 0 0 )
            , ( vec3 2 0 -2, vec3 4 0 -2, vec3 2 0 0, vec3 4 0 0 )
            , ( vec3 4 0 -2, vec3 6 0 -2, vec3 4 0 0, vec3 6 0 0 )
              -- Row 4
            , ( vec3 -6 0 0, vec3 -4 0 0, vec3 -6 0 2, vec3 -4 0 2 )
            , ( vec3 -4 0 0, vec3 -2 0 0, vec3 -4 0 2, vec3 -2 0 2 )
            , ( vec3 -2 0 0, vec3 0 0 0, vec3 -2 0 2, vec3 0 0 2 )
            , ( vec3 0 0 0, vec3 2 0 0, vec3 0 0 2, vec3 2 0 2 )
            , ( vec3 2 0 0, vec3 4 0 0, vec3 2 0 2, vec3 4 0 2 )
            , ( vec3 4 0 0, vec3 6 0 0, vec3 4 0 2, vec3 6 0 2 )
              -- Row 5
            , ( vec3 -6 0 2, vec3 -4 0 2, vec3 -6 0 4, vec3 -4 0 4 )
            , ( vec3 -4 0 2, vec3 -2 0 2, vec3 -4 0 4, vec3 -2 0 4 )
            , ( vec3 -2 0 2, vec3 0 0 2, vec3 -2 0 4, vec3 0 0 4 )
            , ( vec3 0 0 2, vec3 2 0 2, vec3 0 0 4, vec3 2 0 4 )
            , ( vec3 2 0 2, vec3 4 0 2, vec3 2 0 4, vec3 4 0 4 )
            , ( vec3 4 0 2, vec3 6 0 2, vec3 4 0 4, vec3 6 0 4 )
              -- Row 6
            , ( vec3 -6 0 4, vec3 -4 0 4, vec3 -6 0 6, vec3 -4 0 6 )
            , ( vec3 -4 0 4, vec3 -2 0 4, vec3 -4 0 6, vec3 -2 0 6 )
            , ( vec3 -2 0 4, vec3 0 0 4, vec3 -2 0 6, vec3 0 0 6 )
            , ( vec3 0 0 4, vec3 2 0 4, vec3 0 0 6, vec3 2 0 6 )
            , ( vec3 2 0 4, vec3 4 0 4, vec3 2 0 6, vec3 4 0 6 )
            , ( vec3 4 0 4, vec3 6 0 4, vec3 4 0 6, vec3 6 0 6 )
            ]


makeFarWall : Drawable T.Vertex
makeFarWall =
    Triangle <|
        concatMap T.makeFace
            [ -- Bottom row - give room for a door.
              ( vec3 -6 2 -6, vec3 -4 2 -6, vec3 -6 0 -6, vec3 -4 0 -6 )
            , ( vec3 -4 2 -6, vec3 -2 2 -6, vec3 -4 0 -6, vec3 -2 0 -6 )
            , ( vec3 0 2 -6, vec3 2 2 -6, vec3 0 0 -6, vec3 2 0 -6 )
            , ( vec3 2 2 -6, vec3 4 2 -6, vec3 2 0 -6, vec3 4 0 -6 )
            , ( vec3 4 2 -6, vec3 6 2 -6, vec3 4 0 -6, vec3 6 0 -6 )
            ]


makeFarWallDoor : Drawable T.Vertex
makeFarWallDoor =
    Triangle <|
        concatMap T.makeFace
            [ ( vec3 -2 2 -6, vec3 0 2 -6, vec3 -2 0 -6, vec3 0 0 -6 )
            ]


modelView : Chambre -> Mat4
modelView chambre =
    mul (makeTranslate chambre.coord) (makeScale chambre.scale)
