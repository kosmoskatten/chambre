module Pyramid exposing (Pyramid, make, incYaw, setPyramidTile, view)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4
    exposing
        ( Mat4
        , mul
        , makeRotate
        , makeTranslate
        , makeScale
        )
import Textured as T
import WebGL exposing (Drawable(..), Renderable, Texture)


type alias Pyramid =
    { coord : Vec3
    , scale : Vec3
    , yaw : Float
    , mesh : Drawable T.Vertex
    , pyramidTile : Maybe Texture
    }


make : Vec3 -> Vec3 -> Pyramid
make coord scale =
    { coord = coord
    , scale = scale
    , yaw = pi / 8
    , mesh = makePyramid
    , pyramidTile = Nothing
    }


incYaw : Float -> Pyramid -> Pyramid
incYaw theta pyramid =
    { pyramid | yaw = pyramid.yaw + theta }


setPyramidTile : Texture -> Pyramid -> Pyramid
setPyramidTile texture mesh =
    { mesh | pyramidTile = Just texture }


view : Mat4 -> Pyramid -> List Renderable
view perspective pyramid =
    case pyramid.pyramidTile of
        Just texture ->
            [ WebGL.render
                T.vertexShader
                T.fragmentShader
                pyramid.mesh
                { perspective = perspective
                , modelView = modelView pyramid
                , texture = texture
                }
            ]

        Nothing ->
            []


makePyramid : Drawable T.Vertex
makePyramid =
    --    Triangle <| T.makeFace ( vec3 -1 1 0, vec3 1 1 0, vec3 -1 -1 0, vec3 1 -1 0 )
    Triangle
        [ ( T.Vertex (vec3 0 1 0) (vec3 0.5 1 0)
          , T.Vertex (vec3 1 -1 1) (vec3 1 0 0)
          , T.Vertex (vec3 -1 -1 1) (vec3 0 0 0)
          )
        ]


modelView : Pyramid -> Mat4
modelView pyramid =
    mul (makeTranslate pyramid.coord) <|
        mul (makeRotate pyramid.yaw <| vec3 0 1 0)
            (makeScale pyramid.scale)
