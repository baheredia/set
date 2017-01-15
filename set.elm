import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random.List
import Random

import Extras exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

init_table : Table
init_table = [Nothing, Nothing, Nothing, Nothing
             ,Nothing, Nothing, Nothing, Nothing
             ,Nothing, Nothing, Nothing, Nothing]

init_selection : List Bool
init_selection = [False,False,False,False
                 ,False,False,False,False
                 ,False,False,False,False
                 ]

init : (Model, Cmd Msg)
init =  ({deck = []
          --deck = List.drop 75 init_deck   --This is for testing
         ,table = init_table
         ,selection = init_selection
         ,score = 0
         ,mode = Start
         }
        , Cmd.none)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Shuffle deck ->
            ({ model | mode = Game }
            ,Random.generate PutDeck (Random.List.shuffle deck))

        PutDeck shuffled_deck ->
            ({ model |
                   deck = Tuple.first <| dealCards shuffled_deck model.table
                   ,table = Tuple.second <| dealCards shuffled_deck model.table
             }
            ,Cmd.none)
        Select n ->
            if (howManyTrue (switchElement n model.selection)) <= 3
            then
                ({ model |
                       selection = (switchElement n model.selection)
                 }
                ,Cmd.none)
            else
                (model, Cmd.none)
        Set ->
            if List.length model.table <= 12
            then
                ({ model |
                       deck =
                           Tuple.first
                               <| dealCards model.deck
                               <| takeSetOut model.table model.selection
                       ,table =
                           Tuple.second
                               <| dealCards model.deck
                               <| takeSetOut model.table model.selection
                       ,selection = init_selection
                       ,score = model.score + 1
                 }                         
                , Cmd.none)
            else
                ({ model |
                       deck = model.deck
                       ,table =
                           filteredBy (List.map not model.selection)
                               <| model.table
                       ,selection =
                           filteredBy (List.map not model.selection)
                               <| model.selection
                       ,score = model.score + 1
                 }
                , Cmd.none)
        ExtraCard ->
            ({ model |
                   deck =
                       Tuple.first
                           <| dealCards model.deck
                           <| model.table ++ [Nothing,Nothing,Nothing]
                   ,table =
                       Tuple.second
                           <| dealCards model.deck
                           <| model.table ++ [Nothing,Nothing,Nothing]
                   ,selection = model.selection ++ [False,False,False]
             }
            , Cmd.none
            )
        Reset -> ({deck = []
                  ,table = init_table
                  ,selection = init_selection
                  ,score = 0
                  ,mode = Start
                  }
                 ,Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW


view : Model -> Html Msg
view model =
    case model.mode of
        Start ->
            div []
                [ h1 [] [text "Bienvenido a Set, ¿Listo para jugar?"]
                , button [onClick (Shuffle (initialDeck False))]
                    [text "¡Empieza!"]
                , button [onClick (Shuffle (initialDeck True))]
                    [text "Sólo quiero un color"]
                ]
        Game ->
            div []
                [ putCard 0 model.table model.selection
                , putCard 1 model.table model.selection
                , putCard 2 model.table model.selection
                , putCard 3 model.table model.selection
                , extraCard 0 model.table model.selection
                , extraCard 3 model.table model.selection
                , setButton (isListSet (filteredBy model.selection model.table))
                , div [style [("background-color", "pink")
                             ,("display", "inline-flex")
                             ,("width","80px")
                             ,("height","40px")
                             ,("align-items","center")
                             ,("justify-content","center")
                             ,("position","relative")
                             ,("left","40px")]
                      ]
                      [text ("Puntos:\n " ++ toString model.score)]
                , br [] []
                , putCard 4 model.table model.selection
                , putCard 5 model.table model.selection
                , putCard 6 model.table model.selection
                , putCard 7 model.table model.selection
                , extraCard 1 model.table model.selection
                , extraCard 4 model.table model.selection
                , addMoreCards model.table model.deck
                , br [] []
                , putCard 8 model.table model.selection
                , putCard 9 model.table model.selection
                , putCard 10 model.table model.selection
                , putCard 11 model.table model.selection
                , extraCard 2 model.table model.selection
                , extraCard 5 model.table model.selection
                , button [onClick Reset] [text "Reset"]
                ]

