module Scene exposing (Model, Msg, init, update, view, subscriptions)

import AnimationFrame exposing (diffs)
import Chambre exposing (Chambre)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr
import Math.Vector3 exposing (vec3)
import Math.Matrix4 exposing (Mat4, makePerspective)
import Pyramid exposing (Pyramid)
import Task as Task
import Time exposing (Time, inSeconds)
import WebGL as WebGL


type alias Model =
    { perspective : Mat4
    , chambre : Chambre
    , pyramid : Pyramid
    , errMsg : String
    }


type Msg
    = Animate Time
    | TextureLoaded (List WebGL.Texture)
    | TextureFailed WebGL.Error


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , chambre = Chambre.make (vec3 0 -5 -60) (vec3 1 1 1)
      , pyramid = Pyramid.make (vec3 0 0 -6) (vec3 1 1 1)
      , errMsg = ""
      }
    , tryLoadTextures
        [ "textures/floor-tile.jpg"
        , "textures/allseeing-eye.png"
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( { model
                | pyramid =
                    Pyramid.incYaw (inSeconds dt * pi / 4) model.pyramid
              }
            , Cmd.none
            )

        TextureLoaded [ floorTile, pyramidTile ] ->
            ( { model
                | chambre = Chambre.setFloorTile floorTile model.chambre
                , pyramid = Pyramid.setPyramidTile pyramidTile model.pyramid
              }
            , Cmd.none
            )

        TextureLoaded _ ->
            ( { model | errMsg = "Unexpected number of textures" }, Cmd.none )

        TextureFailed _ ->
            ( { model | errMsg = "TextureFailed" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewScene model
        , p [] [ text model.errMsg ]
        ]


viewScene : Model -> Html Msg
viewScene model =
    WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] <|
        Chambre.view model.perspective model.chambre
            ++ Pyramid.view model.perspective model.pyramid


subscriptions : Model -> Sub Msg
subscriptions model =
    diffs Animate


tryLoadTextures : List String -> Cmd Msg
tryLoadTextures urls =
    Task.attempt
        (\result ->
            case result of
                Ok value ->
                    TextureLoaded value

                Err err ->
                    TextureFailed err
        )
    <|
        Task.sequence (List.map WebGL.loadTexture urls)


sceneWidth : Int
sceneWidth =
    500


sceneHeight : Int
sceneHeight =
    400
