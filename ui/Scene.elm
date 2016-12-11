module Scene exposing (Model, Msg, init, update, view, subscriptions)

import Chambre exposing (Chambre)
import Html exposing (Html, p, text)
import Html.Attributes as Attr
import Math.Vector3 exposing (vec3)
import Math.Matrix4 exposing (Mat4, makePerspective)
import WebGL as WebGL


type alias Model =
    { perspective : Mat4
    , chambre : Chambre
    }


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , chambre = Chambre.make (vec3 0 -2 -10) (vec3 1 1 1)
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] <| Chambre.view model.perspective model.chambre


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


sceneWidth : Int
sceneWidth =
    500


sceneHeight : Int
sceneHeight =
    400
