import Html exposing (Html, button, div, input)
import Html.Events exposing (on)
import String
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (style)
import Time exposing (Time, second, millisecond)
import Keyboard
import Debug exposing (log)

gameHeight : Float
gameHeight = 200
gameWidth : Float
gameWidth = 600
playerX : Float
playerX = 200
playerSize : Float
playerSize = 20
playerJumpSpeed : Float
playerJumpSpeed = 4
spikeMoveSpeed : Float
spikeMoveSpeed = 3
maxJump : Float
maxJump = 100
floor : Float
floor = gameHeight - playerSize

main : Program Never
main =
    App.program { init = init, view = view, update = update,
    subscriptions = subscriptions }

-- MODEL

type alias Position =
    { x: Float, y: Float }

type alias Player =
    { playerHeight : Float, jumping : Bool }

type alias Spike =
    { position : Position }

type alias Score =
    Int

type Model =
    NotStarted Score | Started Player (List Spike) Score


-- init

init : (Model, Cmd Msg)
init =
    (NotStarted 0, Cmd.none)



-- UPDATE

type Msg =
    Tick Time | KeyPress Keyboard.KeyCode | AddSpike

checkCollision : Player -> Spike -> Bool
checkCollision player spike =
    let
        inX = (playerX > spike.position.x) && (playerX < spike.position.x + playerSize)
        inY = (player.playerHeight + playerSize > spike.position.y) && (player.playerHeight < spike.position.y + playerSize)
    in
        inX && inY

addSpike : List Spike -> List Spike
addSpike spikes =
    let
        spikes = List.map (\spike -> { position={ x=spike.position.x-spikeMoveSpeed, y=spike.position.y } }) spikes
        lastSpike = case List.head spikes of
            Nothing ->
                { position={ x=0, y=0 } }
            Just val ->
                val
        lastSpikeX = lastSpike.position.x
    in
        if lastSpikeX < gameWidth-playerSize-300
            then List.append [{ position={ x=gameWidth, y=floor }}] spikes
        else
            spikes


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        NotStarted score ->
            case msg of
                Tick _ ->
                    ( model, Cmd.none )
                KeyPress key ->
                    if key == 32 then ( Started (Player floor False) [] 0, Cmd.none )
                    else ( model, Cmd.none )
                AddSpike ->
                    ( model, Cmd.none )
        Started player spikes score ->
            let
                checkPlayerCollision = checkCollision player
                collisions = List.map checkPlayerCollision spikes
                isGameOver = List.member True collisions

                playerHeight = player.playerHeight
                jumping = (
                    if not player.jumping || player.playerHeight <= maxJump then
                        False
                    else True
                )
                newHeight = (
                    if jumping then playerHeight - playerJumpSpeed
                    else if playerHeight < floor then playerHeight + playerJumpSpeed
                    else playerHeight
                )
            in
                case msg of
                    Tick newTime ->
                        if isGameOver then
                            ( NotStarted score, Cmd.none )
                        else
                        (
                            Started { player | playerHeight = newHeight, jumping = jumping }
                            (addSpike spikes) (score + 1),
                            Cmd.none
                        )
                    KeyPress key ->
                        if key == 32 && playerHeight == floor then
                            ( Started { player | jumping = True } spikes score, Cmd.none )
                        else (Started player spikes score, Cmd.none)
                    AddSpike ->
                        ( model, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch([
        Keyboard.presses KeyPress,
        Time.every (millisecond*20) Tick
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
        pointMid = toString (playerSize / 2) ++ ",0"
        pointList = [pointLeft, pointMid, pointRight]
    in
        polygon [width (toString playerSize), height (toString playerSize), fill "white",
                 transform ("translate(" ++ toString(spike.position.x) ++ "," ++ toString(spike.position.y) ++ ")"),
                 points (String.join " " pointList)] []

getRotation : Float -> Float -> Float -> String
getRotation deg x y =
    "rotate(" ++ toString(deg) ++ " " ++ toString(x) ++ " " ++ toString(y) ++ ")"

playerRect : Player -> Html a
playerRect player =
    let
        playerCenterX = playerX + playerSize/2
        playerCenterY = player.playerHeight + playerSize/2
        rotationValue = ( 180 / (floor - (maxJump)) ) * (floor - player.playerHeight)
        rotation =
            if player.jumping then rotationValue
            else -rotationValue

    in
        rect [width (toString playerSize), height (toString playerSize), fill "white",
              transform ( getRotation rotation playerCenterX playerCenterY ),
              y (toString player.playerHeight), x (toString playerX)] []

gameContainer : List (Html msg) -> Html msg
gameContainer children =
    div [ Html.Attributes.style containerStyle ]
        [
            svg [
                width (toString gameWidth),
                height (toString gameHeight),
                fontFamily "Verdana"
            ]
            (List.concat [
                [rect [width "100%", height "100%", fill "black"] []],
                children
            ])
        ]

scoreText : Score -> Bool -> Html a
scoreText score started =
    let
        anchor = if started then
                "start"
            else
                "middle"
        xVal = if started then
                toString 10
            else
                toString (gameWidth/2)
        yVal = if started then
                toString 20
            else
                toString (gameHeight/2 + 50)
    in
        text' [x xVal,
               y yVal,
               textAnchor anchor,
               fill "white"
              ] [ text ("Score: " ++ toString score) ]

view : Model -> Html Msg
view model =
    case model of
        NotStarted score ->
            gameContainer [
                text' [x (toString (gameWidth/2)),
                       y (toString (gameHeight/2)),
                       fill "white",
                       textAnchor "middle"
                      ] [ text "Press \"SPACE\" to start game" ],
                scoreText score False
            ]
        Started player spikes score ->
                gameContainer
                    (List.concat [
                        [
                            playerRect player,
                            scoreText score True
                        ],
                        List.map spikeTriangle spikes
                    ])
