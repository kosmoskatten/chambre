module Scene exposing (Model, Msg, init, update, view, subscriptions)

import AnimationFrame exposing (diffs)
import Chambre exposing (Chambre)
import Html exposing (Html, div, p, fieldset, input, label, text, table, tr, td)
import Html.Attributes as Attr
import Html.Events as Evts
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


type CameraPos
    = DownInCorner
    | UpInCorner
    | DownAtSide
    | UpAtSide


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , chambre = Chambre.make (vec3 0 0 -50) (vec3 5 5 5)
      , pyramid = Pyramid.make (vec3 0 6 -50) (vec3 5 5 5)
      , cameraPos = DownInCorner
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

        SwitchTo pos ->
            ( { model | cameraPos = pos }, Cmd.none )

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
    div [ Attr.style [ ( "width", "800px" ), ( "border", "solid" ) ] ]
        [ p [] [ text "Camera Positions" ]
        , table []
            [ tr [] [ td [] [ radio "Down in corner" <| SwitchTo DownInCorner ] ]
            , tr [] [ td [] [ radio "Up in corner" <| SwitchTo UpInCorner ] ]
            , tr [] [ td [] [ radio "Down at side" <| SwitchTo DownAtSide ] ]
            , tr [] [ td [] [ radio "Up at side" <| SwitchTo UpAtSide ] ]
            ]
        ]


radio : String -> Msg -> Html Msg
radio value msg =
    label [ Attr.style [ ( "padding", "20px" ) ] ]
        [ input [ Attr.type_ "radio", Attr.name "camera", Evts.onClick msg ]
            []
        , text value
        ]


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


getCameraPos : Model -> ( Vec3, Vec3 )
getCameraPos model =
    case model.cameraPos of
        DownInCorner ->
            ( vec3 -34 2 -79, vec3 0 10 -50 )

        UpInCorner ->
            ( vec3 -34 20 -79, vec3 0 3 -50 )

        DownAtSide ->
            ( vec3 0 2 -84, vec3 0 10 -50 )

        UpAtSide ->
            ( vec3 0 20 -84, vec3 0 3 -50 )


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
