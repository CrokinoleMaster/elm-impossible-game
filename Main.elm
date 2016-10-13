import Html exposing (Html, button, div, text, input)
import Html.Events exposing (on)
import String
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second, millisecond)
import Keyboard

gameHeight : Int
gameHeight = 200
gameWidth : Int
gameWidth = 600
playerX : Int
playerX = 200
playerSize : Int
playerSize = 20
maxJump : Int
maxJump = 100
floor : Int
floor = gameHeight - playerSize

main : Program Never
main =
    App.program { init = init, view = view, update = update,
    subscriptions = subscriptions }

-- MODEL

type alias Position =
    ( Int, Int )

type alias Player =
    { playerHeight : Int, jumping : Bool }

type alias Model =
    { playerHeight : Int,
      jumping : Bool}


-- init
init : (Model, Cmd Msg)
init =
    ( Model floor False, Cmd.none )



-- UPDATE

type Msg =
    Tick Time | KeyPress Keyboard.KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        playerHeight = model.playerHeight
        jumping = (
            if not model.jumping || model.playerHeight <= maxJump then
                False
            else True
        )
        newHeight = (
            if jumping then playerHeight - 1
            else if playerHeight < floor then playerHeight + 1
            else playerHeight
        )
    in
        case msg of
            Tick newTime ->
                (
                    { model | playerHeight = newHeight, jumping = jumping },
                    Cmd.none
                )
            KeyPress key ->
                if key == 32 && playerHeight == floor then
                    ({ model | jumping = True }, Cmd.none)
                else (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch([
        Keyboard.presses KeyPress,
        Time.every (millisecond*5) Tick
    ])


-- VIEW

player : Model -> Html a
player model =
    rect [width (toString playerSize), height (toString playerSize), fill "white",
          y (toString model.playerHeight), x (toString playerX)] []

view : Model -> Html Msg
view model =
    div []
        [
            svg [width (toString gameWidth), height (toString gameHeight)] [
                rect [width "100%", height "100%", fill "black"] [],
                player model
            ]
        ]


