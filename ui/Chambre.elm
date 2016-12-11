module Chambre exposing (Chambre, make, setFloorTile, view)

import Colored as C
import List exposing (concatMap)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (vec4)
import Math.Matrix4 exposing (Mat4, mul, makeTranslate, makeScale)
import Textured as T
import WebGL exposing (Drawable(..), Renderable, Texture)


type alias Chambre =
    { coord : Vec3
    , scale : Vec3
    , floor : Drawable T.Vertex
    , floorTile : Maybe Texture
    }


make : Vec3 -> Vec3 -> Chambre
make coord scale =
    { coord = coord
    , scale = scale
    , floor = makeFloor
    , floorTile = Nothing
    }


setFloorTile : Texture -> Chambre -> Chambre
setFloorTile texture chambre =
    { chambre | floorTile = Just texture }


view : Mat4 -> Chambre -> List Renderable
view perspective chambre =
    case chambre.floorTile of
        Just texture ->
            [ WebGL.render T.vertexShader T.fragmentShader chambre.floor { perspective = perspective, modelView = modelView chambre, texture = texture }
            ]

        Nothing ->
            []



{- }
   makeFloor : Drawable C.Vertex
   makeFloor =
       Triangle <|
           concatMap C.makeFace
               [ ( ( vec3 -2 0 -2, vec3 0 0 -2, vec3 -2 0 0, vec3 0 0 0 ), vec4 1 0 0 1 )
               , ( ( vec3 0 0 -2, vec3 2 0 -2, vec3 0 0 0, vec3 2 0 0 ), vec4 0 1 0 1 )
               , ( ( vec3 -2 0 0, vec3 0 0 0, vec3 -2 0 2, vec3 0 0 2 ), vec4 0 0 1 1 )
               , ( ( vec3 0 0 0, vec3 2 0 0, vec3 0 0 2, vec3 2 0 2 ), vec4 1 1 0 1 )
               ]
-}


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


modelView : Chambre -> Mat4
modelView chambre =
    mul (makeTranslate chambre.coord) (makeScale chambre.scale)
