module Chambre exposing (Chambre, make, view)

import Colored as C
import List exposing (concatMap)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (vec4)
import Math.Matrix4 exposing (Mat4, mul, makeTranslate, makeScale)
import WebGL exposing (Drawable(..), Renderable)


type alias Chambre =
    { coord : Vec3
    , scale : Vec3
    , floor : Drawable C.Vertex
    }


make : Vec3 -> Vec3 -> Chambre
make coord scale =
    { coord = coord
    , scale = scale
    , floor = makeFloor
    }


view : Mat4 -> Chambre -> List Renderable
view perspective chambre =
    [ WebGL.render C.vertexShader C.fragmentShader chambre.floor { perspective = perspective, modelView = modelView chambre }
    ]


makeFloor : Drawable C.Vertex
makeFloor =
    Triangle <|
        concatMap C.makeFace
            [ ( ( vec3 -2 0 -2, vec3 0 0 -2, vec3 -2 0 0, vec3 0 0 0 ), vec4 1 0 0 1 )
            , ( ( vec3 0 0 -2, vec3 2 0 -2, vec3 0 0 0, vec3 2 0 0 ), vec4 0 1 0 1 )
            , ( ( vec3 -2 0 0, vec3 0 0 0, vec3 -2 0 2, vec3 0 0 2 ), vec4 0 0 1 1 )
            , ( ( vec3 0 0 0, vec3 2 0 0, vec3 0 0 2, vec3 2 0 2 ), vec4 1 1 0 1 )
            ]



--Triangle <| C.makeFace ( ( vec3 -1 1 -1, vec3 1 1 -1, vec3 -1 -1 -1, vec3 1 -1 -1 ), vec4 1 0 0 1 )


modelView : Chambre -> Mat4
modelView chambre =
    mul (makeTranslate chambre.coord) (makeScale chambre.scale)
