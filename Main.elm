import Html exposing (Html, button, div, input)
import Html.Events exposing (on)
import String
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (style)
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
    { x: Int, y: Int }

type alias Player =
    { playerHeight : Int, jumping : Bool }

type alias Spike =
    { position : Position }

type Model =
    NotStarted | Started Player (List Spike)


-- init

init : (Model, Cmd Msg)
init =
    (NotStarted, Cmd.none)



-- UPDATE

type Msg =
    Tick Time | KeyPress Keyboard.KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        NotStarted ->
            case msg of
                Tick _ ->
                    ( model, Cmd.none )
                KeyPress key ->
                    if key == 32 then ( Started (Player floor False) [], Cmd.none )
                    else ( model, Cmd.none )
        Started player spikes->
            let
                playerHeight = player.playerHeight
                jumping = (
                    if not player.jumping || player.playerHeight <= maxJump then
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
                            Started { player | playerHeight = newHeight, jumping = jumping }
                            [{ position={ x=100, y=100 } }],
                            Cmd.none
                        )
                    KeyPress key ->
                        if key == 32 && playerHeight == floor then
                            ( Started { player | jumping = True } spikes, Cmd.none )
                        else (Started player spikes, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch([
        Keyboard.presses KeyPress,
        Time.every (millisecond*2) Tick
    ])



-- STYLES

containerStyle : List (String, String)
containerStyle =
    [
        ("display", "flex"),
        ("align-items", "center"),
        ("justify-content", "center"),
        ("height", "100%")
    ]


-- VIEW

spikeTriangle : Spike -> Html a
spikeTriangle spike =
    let
        pointLeft = "0," ++ toString playerSize
        pointRight = toString playerSize ++ "," ++ toString playerSize
        pointMid = toString (toFloat playerSize / 2) ++ ",0"
        pointList = [pointLeft, pointMid, pointRight]
    in
        polygon [width (toString playerSize), height (toString playerSize), fill "white",
                 transform ("translate(" ++ toString(spike.position.x) ++ "," ++ toString(spike.position.y) ++ ")"),
                 points (String.join " " pointList)] []

getRotation : Float -> Int -> Int -> String
getRotation deg x y =
    "rotate(" ++ toString(deg) ++ " " ++ toString(x) ++ " " ++ toString(y) ++ ")"

playerRect : Player -> Html a
playerRect player =
    let
        playerCenterX = playerX + playerSize//2
        playerCenterY = player.playerHeight + playerSize//2
        rotationValue = ( 180 / toFloat(floor - (maxJump)) ) * toFloat(floor - player.playerHeight)
        rotation =
            if player.jumping then rotationValue
            else -rotationValue

    in
        rect [width (toString playerSize), height (toString playerSize), fill "white",
              transform ( getRotation rotation playerCenterX playerCenterY ),
              y (toString player.playerHeight), x (toString playerX)] []

view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            div [ Html.Attributes.style containerStyle ]
                [
                    svg [width (toString gameWidth), height (toString gameHeight)] [
                        rect [width "100%", height "100%", fill "black"] [],
                        text' [x (toString (gameWidth//2)),
                               y (toString (gameHeight//2)),
                               fill "white",
                               textAnchor "middle",
                               fontFamily "Verdana"
                              ] [ text "Press \"SPACE\" to start game" ]
                    ]
                ]
        Started player spikes->
            div [ Html.Attributes.style containerStyle ]
                [
                    svg [width (toString gameWidth), height (toString gameHeight)]
                    (List.concat [
                        [
                            rect [width "100%", height "100%", fill "black"] [],
                            playerRect player
                        ],
                        List.map spikeTriangle spikes
                    ])
                ]
