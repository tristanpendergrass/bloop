module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { game : Maybe Game
    , prevGames : List GameOutcome
    }


type alias Game =
    { oxygen : Int
    , treasure : List Treasure
    , tiles : List Tile
    , playerLocation : PlayerLocation
    , playerDirection : Direction
    , prevRoll : Maybe Int
    }


type PlayerLocation
    = Submarine
    | Water Int


type Treasure
    = Treasure Int


type Tile
    = TreasureTile Treasure
    | EmptyTile


type Direction
    = Up
    | Down


type GameOutcome
    = Drowned (List Treasure)
    | Survived { treasure : List Treasure }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Nothing, prevGames = [] }, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | HandleStartGamePress
    | HandleStartGameResult Game
    | HandleTurnAroundPress
    | HandleRollPress
    | HandleRollResult Int
    | HandlePickUpPress


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleStartGamePress ->
            ( model, Random.generate HandleStartGameResult gameGenerator )

        HandleStartGameResult gameResult ->
            ( { model | game = Just gameResult }, Cmd.none )

        HandleTurnAroundPress ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    ( { model | game = Just { game | playerDirection = Up } }, Cmd.none )

        HandleRollPress ->
            ( model, Random.generate HandleRollResult (Random.int 2 6) )

        HandleRollResult roll ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    let
                        adjustedRoll =
                            roll
                                |> adjustForWeight game
                                |> adjustForDirection game

                        newLocation =
                            movePlayer game adjustedRoll game.playerLocation

                        newOxygen =
                            game.oxygen - List.length game.treasure
                    in
                    if newLocation == Submarine && game.playerDirection == Up then
                        ( { model | game = Nothing, prevGames = Survived { treasure = game.treasure } :: model.prevGames }, Cmd.none )

                    else if newOxygen <= 0 && newLocation /= Submarine then
                        ( { model | game = Nothing, prevGames = Drowned game.treasure :: model.prevGames }, Cmd.none )

                    else
                        ( { model | game = Just { game | prevRoll = Just roll, playerLocation = newLocation, oxygen = newOxygen } }, Cmd.none )

        HandlePickUpPress ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    case game.playerLocation of
                        Submarine ->
                            ( model, Cmd.none )

                        Water idx ->
                            case List.drop idx game.tiles |> List.head of
                                Just (TreasureTile treasure) ->
                                    let
                                        newTiles =
                                            List.indexedMap
                                                (\i t ->
                                                    if i == idx then
                                                        EmptyTile

                                                    else
                                                        t
                                                )
                                                game.tiles

                                        newTreasure =
                                            treasure :: game.treasure

                                        newGame =
                                            { game | tiles = newTiles, treasure = newTreasure }
                                    in
                                    ( { model | game = Just newGame }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )


adjustForDirection : Game -> Int -> Int
adjustForDirection game roll =
    if game.playerDirection == Down then
        roll

    else
        -1 * roll


adjustForWeight : Game -> Int -> Int
adjustForWeight game roll =
    roll - List.length game.treasure


movePlayer : Game -> Int -> PlayerLocation -> PlayerLocation
movePlayer game amount location =
    case location of
        Submarine ->
            Water amount

        Water num ->
            if num + amount < 0 then
                Submarine

            else if num + amount > List.length game.tiles - 1 then
                Water (List.length game.tiles - 1)

            else
                Water (num + amount)


gameGenerator : Random.Generator Game
gameGenerator =
    Random.map createGame tilesGenerator


tilesGenerator : Random.Generator (List Tile)
tilesGenerator =
    let
        smallTreasure =
            Random.list 5 (tileGenerator 0 3)

        mediumTreasure =
            Random.list 5 (tileGenerator 4 6)

        largeTreasure =
            Random.list 5 (tileGenerator 7 9)
    in
    Random.map3 (\a b c -> a ++ b ++ c) smallTreasure mediumTreasure largeTreasure


createGame : List Tile -> Game
createGame tiles =
    { oxygen = 20
    , treasure = []
    , tiles = tiles
    , playerLocation = Submarine
    , playerDirection = Down
    , prevRoll = Nothing
    }


tileGenerator : Int -> Int -> Random.Generator Tile
tileGenerator min max =
    Random.map (Treasure >> TreasureTile) (Random.int min max)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []



-- VIEW


proseClass : Attribute Msg
proseClass =
    class "prose prose-sm md:prose-base"


view : Model -> Html Msg
view model =
    case model.game of
        Nothing ->
            let
                renderOutcome outcome =
                    case outcome of
                        Survived { treasure } ->
                            let
                                score =
                                    List.sum (List.map (\(Treasure v) -> v) treasure)
                            in
                            div []
                                [ span [ class "font-bold text-green-700" ] [ text ("Survived! Score: " ++ String.fromInt score) ]
                                ]

                        Drowned treasure ->
                            let
                                score =
                                    List.sum (List.map (\(Treasure v) -> v) treasure)
                            in
                            div []
                                [ span [ class "text-red-500" ] [ text ("Drowned. Score: " ++ String.fromInt score) ]
                                ]
            in
            div []
                [ button [ class "btn", onClick HandleStartGamePress ] [ text "Start Game" ]
                , case model.prevGames of
                    [] ->
                        text ""

                    first :: rest ->
                        div [ class "mt-6" ]
                            [ h2 [ class "text-xl font-bold bg-blue-100 p-4" ] [ text "Game Result" ]
                            , div [ class "bg-blue-100 p-4" ] [ renderOutcome first ]
                            , if List.isEmpty rest then
                                text ""

                              else
                                div [ class "divider my-4" ] []
                            , if List.isEmpty rest then
                                text ""

                              else
                                div []
                                    [ h3 [ class "text-lg font-semibold mb-2" ] [ text "Previous Games" ]
                                    , div [] (List.map renderOutcome rest)
                                    ]
                            ]
                ]

        Just game ->
            let
                ( onTreasureTile, onSubmarine ) =
                    case game.playerLocation of
                        Submarine ->
                            ( False, True )

                        Water idx ->
                            case List.drop idx game.tiles |> List.head of
                                Just (TreasureTile _) ->
                                    ( True, False )

                                _ ->
                                    ( False, False )
            in
            div []
                [ div [] [ text "Oxygen: ", strong [] [ text (String.fromInt game.oxygen) ] ]
                , div [] [ text "Weight: ", strong [] [ text (String.fromInt (List.length game.treasure)) ] ]
                , div [ class "flex flex-row gap-2" ] (text "Inventory: " :: List.map renderTreasure game.treasure)
                , div [ class "divider" ] []
                , div [ class "flex items-center gap-4" ]
                    [ button [ class "btn btn-sm btn-primary", onClick HandleRollPress ] [ text "Roll" ]
                    , button [ class "btn btn-sm btn-secondary", onClick HandleTurnAroundPress, disabled (game.playerDirection == Up) ] [ text "Go back up" ]
                    , button [ class "btn btn-sm btn-accent", onClick HandlePickUpPress, disabled (onSubmarine || not onTreasureTile) ] [ text "Pick Up" ]
                    , case game.prevRoll of
                        Nothing ->
                            text ""

                        Just roll ->
                            text ("Rolled: " ++ String.fromInt roll)
                    ]
                , ul [] (List.indexedMap (\index tile -> li [] [ renderTile game index tile ]) game.tiles)
                ]


renderTile : Game -> Int -> Tile -> Html Msg
renderTile game index tile =
    let
        isPlayerLocation =
            game.playerLocation == Water index

        playerLocationEl =
            if isPlayerLocation && game.playerDirection == Down then
                span [ class "text-blue-500" ] [ text "👤" ]

            else if isPlayerLocation && game.playerDirection == Up then
                span [ class "text-blue-500" ] [ text "👤👆" ]

            else
                span [] []
    in
    case tile of
        TreasureTile _ ->
            span [] [ text "Treasure (??)", playerLocationEl ]

        EmptyTile ->
            span [] [ text "Empty", playerLocationEl ]


renderTreasure : Treasure -> Html Msg
renderTreasure (Treasure value) =
    span [] [ text "Treasure (", strong [] [ text (String.fromInt value) ], text ")" ]
