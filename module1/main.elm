module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (polygon, rect, svg, line)
import Svg.Attributes exposing (version, viewBox, points, fill, transform, x, y, x1, y1, x2, y2, width, height, style)
import Html.App as App
import Html.Events exposing (onClick)
import Task
import VirtualDom
import Window
import Keyboard exposing (KeyCode)
import List exposing (map)
import String exposing (join)
import Json.Decode as Json exposing ((:=))
import Math.Vector2 as V2


type alias Position =
    { x : Int, y : Int }


type alias CirclePosition =
    { position : Position, r : Int }


type Mode
    = Circle
    | Polygon
    | CreateCircle


type alias Model =
    { size : Window.Size
    , pos : Position
    , clickPos : Position
    , points : List Position
    , circles : List CirclePosition
    , mode : Mode
    }


marginScene =
    20


type Msg
    = Error
    | WindowSize Window.Size
    | MouseMove Position
    | Clicked Position
    | KeyPress Keyboard.KeyCode


main : Program Never
main =
    App.program { init = init, update = update, view = view, subscriptions = subscriptions }


init : ( Model, Cmd Msg )
init =
    ( { size = Window.Size 600 600
      , pos = Position 0 0
      , clickPos = Position 0 0
      , points = []
      , circles = []
      , mode = Polygon
      }
    , Task.perform (\_ -> Debug.crash "task") WindowSize Window.size
    )


px : a -> String
px n =
    toString n ++ "px"


toggleMode : Mode -> Mode
toggleMode mode =
    case mode of
        Circle ->
            Polygon

        Polygon ->
            Circle

        CreateCircle ->
            CreateCircle


calculateRadius : Position -> Position -> Int
calculateRadius pos1 pos2 =
    let
        v1 =
            (V2.vec2 (toFloat pos1.x) (toFloat pos1.y))

        v2 =
            (V2.vec2 (toFloat pos2.x) (toFloat pos2.y))
    in
        (Debug.log ((toString (V2.distance v1 v2)) ++ " " ++ (toString pos1) ++ (toString pos2)))
            (round
                (V2.distance v1 v2)
            )


newCircle : Position -> Position -> CirclePosition
newCircle pos1 pos2 =
    { position = pos1, r = (calculateRadius pos1 pos2) }


handleClick : Model -> Position -> ( Model, Cmd Msg )
handleClick model pos =
    let
        mod =
            model
    in
        case mod.mode of
            CreateCircle ->
                ( { mod | circles = (newCircle mod.clickPos pos) :: mod.circles, clickPos = pos, mode = Circle }, Cmd.none )

            Polygon ->
                ( { mod | points = pos :: mod.points }, Cmd.none )

            _ ->
                ( { mod | clickPos = pos }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        WindowSize { width, height } ->
            ( { model | size = Window.Size (width - 2 * marginScene) (height - 100 - 2 * marginScene) }, Cmd.none )

        MouseMove pos ->
            ( { model | pos = pos }, Cmd.none )

        Clicked pos ->
            handleClick model pos

        KeyPress 99 ->
            ( { model | mode = CreateCircle }, Cmd.none )

        KeyPress 110 ->
            ( { model | points = [], circles = [] }, Cmd.none )

        KeyPress 109 ->
            ( { model | mode = toggleMode model.mode }, Cmd.none )

        KeyPress _ ->
            ( model, Cmd.none )

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
        ((background model)
            :: (polygon model)
            :: (circles model)
        )



--[ background model
-- , polygon model
-- , circles model
-- ]


offsetPosition : Json.Decoder Position
offsetPosition =
    Json.object2 Position ("offsetX" := Json.int) ("offsetY" := Json.int)


background : Model -> Svg.Svg Msg
background model =
    Svg.rect
        [ Svg.Attributes.width <| toString model.size.width
        , Svg.Attributes.height <| toString model.size.height
        , fill "white"
        , VirtualDom.on "mousedown" (Json.map Clicked offsetPosition)
        , VirtualDom.on "mousemove" (Json.map MouseMove offsetPosition)
        ]
        []


options =
    { preventDefault = False, stopPropagation = False }


getPoints : Model -> String
getPoints model =
    join " " (map (\p -> toString p.x ++ "," ++ toString p.y) model.points)


polygon : Model -> Svg.Svg Msg
polygon model =
    Svg.polygon
        [ points (getPoints model)
        , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
        ]
        []


toSvgCircle : CirclePosition -> Svg.Svg Msg
toSvgCircle cPos =
    (Svg.circle
        [ Svg.Attributes.cx (toString cPos.position.x), Svg.Attributes.cy (toString cPos.position.y), Svg.Attributes.r (toString cPos.r) ]
        []
    )


circles : Model -> List (Svg.Svg Msg)
circles model =
    map (\cPos -> toSvgCircle cPos) model.circles


subscriptions model =
    Sub.batch
        [ Window.resizes WindowSize
        , Keyboard.presses KeyPress
        ]
