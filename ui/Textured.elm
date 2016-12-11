module Textured
    exposing
        ( Vertex
        , Face
        , makeFace
        , vertexShader
        , fragmentShader
        )

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Drawable(..), Renderable, Shader, Texture)


type alias Vertex =
    { position : Vec3
    , coord : Vec3
    }


type alias Face =
    List ( Vertex, Vertex, Vertex )


makeFace : ( Vec3, Vec3, Vec3, Vec3 ) -> Face
makeFace ( p1, p2, p3, p4 ) =
    [ ( Vertex p1 <| vec3 0 1 0
      , Vertex p2 <| vec3 1 1 0
      , Vertex p3 <| vec3 0 0 0
      )
    , ( Vertex p3 <| vec3 0 0 0
      , Vertex p2 <| vec3 1 1 0
      , Vertex p4 <| vec3 1 0 0
      )
    ]



-- Vertex shader for the Face.


vertexShader :
    Shader
        { attr
            | position : Vec3
            , coord : Vec3
        }
        { unif
            | perspective : Mat4
            , modelView : Mat4
        }
        { vcoord : Vec2 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
uniform mat4 modelView;
varying vec2 vcoord;

void main (void) {
    gl_Position = perspective * modelView * vec4(position, 1.0);
    vcoord = coord.xy;
}

  |]



-- Fragment shader for the Face.


fragmentShader : Shader {} { unif | texture : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|

precision mediump float;

uniform sampler2D texture;
varying vec2 vcoord;

void main (void) {
    gl_FragColor = texture2D(texture, vcoord);
}

    |]
