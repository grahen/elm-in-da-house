import Html exposing (..)
import Html.Attributes exposing (..)

import Svg exposing (polygon, rect, svg, line)
import Svg.Attributes exposing (version, viewBox, points,fill ,transform, x, y,x1,y1, x2,y2, width, height, style)
import Html.App as App
import Html.Events exposing (onClick)
import Task
import VirtualDom
import Window
import List exposing (map)
import String exposing (join)
import Json.Decode as Json exposing ((:=))


type alias Position =
    { x : Int, y : Int }


type alias Model =
    { size : Window.Size
    , pos : Position
    , points : List Position
    }


marginScene =
 20

type Msg
    = Error
    | WindowSize Window.Size
    | MouseMove Position
    | Clicked Position

main : Program Never
main =
    App.program { init = init, update = update, view = view, subscriptions = subscriptions }

init : ( Model, Cmd Msg )
init =
    ( { size = Window.Size 600 600
      , pos = Position 0 0
      , points = []
      }
    , Task.perform (\_ -> Debug.crash "task") WindowSize Window.size
    )

px : a -> String
px n =
    toString n ++ "px"

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        WindowSize { width, height } ->
            ( { model | size = Window.Size (width - 2 * marginScene) (height - 100 - 2 * marginScene) }, Cmd.none )

        MouseMove pos ->
            ( { model | pos = pos }, Cmd.none )

        Clicked pos ->
            ( { model | points = pos :: model.points }, Cmd.none )

        _ ->
            Debug.crash "update"

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text (toString model) ]
        , scene model
        ]


scene : Model -> Html.Html Msg
scene model =
    Svg.svg
        [ Svg.Attributes.width <| toString model.size.width
        , Svg.Attributes.height <| toString model.size.height
        , Svg.Attributes.style ("margin-left:" ++ px marginScene)
        ]
        [ background model
        , tracker model
        ]

offsetPosition : Json.Decoder Position
offsetPosition =
    Json.object2 Position ("offsetX" := Json.int) ("offsetY" := Json.int)



background : Model -> Svg.Svg Msg
background model =
    Svg.rect
        [ Svg.Attributes.width <| toString model.size.width
        , Svg.Attributes.height <| toString model.size.height
        , fill "gray"
        , VirtualDom.on "mousedown" (Json.map Clicked offsetPosition)
  --      , VirtualDom.on "mousemove" (Json.map MouseMove offsetPosition)
        ]
        []


options =
    { preventDefault = False, stopPropagation = False }


getPoints: Model -> String
getPoints model =
 join " " (map (\p -> toString p.x ++ "," ++ toString p.y) model.points)

tracker : Model -> Svg.Svg Msg
tracker model =

   Svg.polygon[points (getPoints model)
        , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
        ]
        []

subscriptions model =
    Window.resizes WindowSize
