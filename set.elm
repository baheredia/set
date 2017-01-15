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

init_deck : Deck
init_deck = initialDeck

init_table : Deck
init_table = [[],[],[],[],[],[],[],[],[],[],[],[]]

init_selection : List Bool
init_selection = [False,False,False,False
                 ,False,False,False,False
                 ,False,False,False,False
                 ]

init : (Model, Cmd Msg)
init =  ({ --deck = init_deck
          deck = List.drop 75 init_deck   --This is for testing
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
        Shuffle ->
            ({ model | mode = Game }
            ,Random.generate PutDeck (Random.List.shuffle model.deck))

        PutDeck shuffled_deck ->
            ({ model |
                   deck = List.drop 12 shuffled_deck
                   ,table = List.take 12 shuffled_deck}
            ,Cmd.none)
        Select n ->
            if (howManyTrue (togleElement n model.selection)) <= 3
            then
                ({ model |
                       selection = (togleElement n model.selection)
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
                               <| putMoreCards model.deck
                               <| dropSet model.table model.selection
                       ,table =
                           Tuple.second
                               <| putMoreCards model.deck
                               <| dropSet model.table model.selection
                       ,selection = init_selection
                       ,score = model.score + 1
                 }                         
                , Cmd.none)
            else
                ({ model |
                       deck = model.deck
                       ,table =
                           escoje (List.map not model.selection)
                               <| model.table
                       ,selection =
                           escoje (List.map not model.selection)
                               <| model.selection
                       ,score = model.score + 1
                 }
                , Cmd.none)
        ExtraCard ->
            ({ model |
                   deck =
                       Tuple.first
                           <| putMoreCards model.deck
                           <| model.table ++ [[],[],[]]
                   ,table =
                       Tuple.second
                           <| putMoreCards model.deck
                           <| model.table ++ [[],[],[]]
                   ,selection = model.selection ++ [False,False,False]
             }
            , Cmd.none
            )
        Reset -> ({deck = init_deck
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
                , button [onClick Shuffle] [text "¡Empieza!"]
                ]
        Game ->
            div []
                [ putCard 0 model
                , putCard 1 model
                , putCard 2 model
                , putCard 3 model
                , extraCard 0 model
                , extraCard 3 model
                , haySet (isListSet (escoje model.selection model.table))
                , div [style [("background-color", "pink")
                             ,("display", "inline-flex")
                             ,("width","100px")
                             ,("height","100px")
                             ,("align-items","center")
                             ,("justify-content","center")
                             ,("position","relative")
                             ,("top","-9px")
                             ,("left","40px")]
                      ]
                      [text ("Llevas " ++ toString model.score ++ " sets")]
                , br [] []
                , putCard 4 model
                , putCard 5 model
                , putCard 6 model
                , putCard 7 model
                , extraCard 1 model
                , extraCard 4 model
                , addMoreCards model.table model.deck
                , br [] []
                , putCard 8 model
                , putCard 9 model
                , putCard 10 model
                , putCard 11 model
                , extraCard 2 model
                , extraCard 5 model
                , button [onClick Reset] [text "Reset"]
                ]

