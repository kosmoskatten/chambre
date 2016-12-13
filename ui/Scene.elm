module Scene exposing (Model, Msg, init, update, view, subscriptions)

import AnimationFrame exposing (diffs)
import Chambre exposing (Chambre)
import Char exposing (fromCode)
import Html exposing (Html, div, p, fieldset, input, label, text, table, tr, td)
import Html.Attributes as Attr
import Keyboard exposing (KeyCode, downs)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul, makePerspective, makeLookAt)
import Pyramid exposing (Pyramid)
import Task as Task
import Time exposing (Time, inSeconds)
import WebGL as WebGL


type alias Model =
    { perspective : Mat4
    , chambre : Chambre
    , pyramid : Pyramid
    , cameraPos : CameraPos
    , errMsg : String
    }


type Msg
    = Animate Time
    | SwitchTo CameraPos
    | TextureLoaded (List WebGL.Texture)
    | TextureFailed WebGL.Error
    | NoOp


type CameraPos
    = One
    | Two
    | Three


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , chambre = Chambre.make (vec3 0 0 -50) (vec3 5 5 5)
      , pyramid = Pyramid.make (vec3 0 6 -50) (vec3 5 5 5)
      , cameraPos = One
      , errMsg = ""
      }
    , tryLoadTextures
        [ "textures/floor-tile.jpg"
        , "textures/allseeing-eye.png"
        , "textures/stone-wall-tile.jpg"
        , "textures/door-tile.jpg"
        , "textures/window-fl1-tile.jpg"
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

        SwitchTo pos ->
            ( { model | cameraPos = pos }, Cmd.none )

        TextureLoaded [ floorTile, pyramidTile, stoneWallTile, doorTile, windowTile ] ->
            ( { model
                | chambre =
                    Chambre.setChambreTiles
                        floorTile
                        stoneWallTile
                        doorTile
                        windowTile
                        model.chambre
                , pyramid = Pyramid.setPyramidTile pyramidTile model.pyramid
              }
            , Cmd.none
            )

        TextureLoaded _ ->
            ( { model | errMsg = "Unexpected number of textures" }, Cmd.none )

        TextureFailed _ ->
            ( { model | errMsg = "TextureFailed" }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewScene model
        , viewCameraControl model
        , p [] [ text model.errMsg ]
        ]


viewScene : Model -> Html Msg
viewScene model =
    let
        vp =
            makeViewerPerspective model
    in
        WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] <|
            Chambre.view vp model.chambre
                ++ Pyramid.view vp model.pyramid


viewCameraControl : Model -> Html Msg
viewCameraControl model =
    div
        [ Attr.style
            [ ( "border", "solid" )
            , ( "width", toString sceneWidth ++ "px" )
            ]
        ]
        [ p []
            [ text <|
                "Scroll through cameras through (1 - 3). Active camera: "
                    ++ getCameraName model
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ diffs Animate, Sub.map handleKey <| downs identity ]



-- Preferring to not push keypresses directly upto the update function. Instead
-- translate to more semantic actions.


handleKey : KeyCode -> Msg
handleKey code =
    case fromCode code of
        '1' ->
            SwitchTo One

        '2' ->
            SwitchTo Two

        '3' ->
            SwitchTo Three

        _ ->
            NoOp


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


getCameraPos : Model -> ( Vec3, Vec3 )
getCameraPos model =
    case model.cameraPos of
        One ->
            ( vec3 -16 15 -16, vec3 0 3 -50 )

        Two ->
            ( vec3 0 15 -16, vec3 0 3 -50 )

        Three ->
            ( vec3 0 95 -49, vec3 0 3 -50 )


getCameraName : Model -> String
getCameraName model =
    case model.cameraPos of
        One ->
            "One"

        Two ->
            "Two"

        Three ->
            "Three"


makeViewerPerspective : Model -> Mat4
makeViewerPerspective model =
    let
        ( eye, lookAt ) =
            getCameraPos model
    in
        mul model.perspective <| makeLookAt eye lookAt (vec3 0 1 0)


sceneWidth : Int
sceneWidth =
    800


sceneHeight : Int
sceneHeight =
    600
