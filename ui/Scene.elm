module Scene exposing (Model, Msg, init, update, view, subscriptions)

import Chambre exposing (Chambre)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr
import Math.Vector3 exposing (vec3)
import Math.Matrix4 exposing (Mat4, makePerspective)
import Task as Task
import WebGL as WebGL


type alias Model =
    { perspective : Mat4
    , chambre : Chambre
    , errMsg : String
    }


type Msg
    = TextureLoaded WebGL.Texture
    | TextureFailed WebGL.Error


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , chambre = Chambre.make (vec3 0 -5 -20) (vec3 1 1 1)
      , errMsg = ""
      }
    , tryLoadTexture "textures/floor-tile.jpg"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded texture ->
            ( { model | chambre = Chambre.setFloorTile texture model.chambre }, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


tryLoadTexture : String -> Cmd Msg
tryLoadTexture url =
    Task.attempt
        (\result ->
            case result of
                Ok value ->
                    TextureLoaded value

                Err err ->
                    TextureFailed err
        )
    <|
        WebGL.loadTexture url


sceneWidth : Int
sceneWidth =
    500


sceneHeight : Int
sceneHeight =
    400
