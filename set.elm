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
init =  ({ deck = []
          --deck = List.drop 75 init_deck   --This is for testing
         , table = init_table
         , selection = init_selection
         , score = 0
         , mode = Start
         , size = 140
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
            let switchedSelection = switchElement n model.selection in
            if howManyTrue switchedSelection <= 3
            then
                if
                    howManyTrue switchedSelection == 3
                    && (isListSet <| filteredBy switchedSelection model.table)
                then
                    if List.length model.table <= 12
                    then
                        ({ model |
                               deck =
                               Tuple.first
                                   <| dealCards model.deck
                                   <| takeSetOut model.table switchedSelection
                         ,table =
                             Tuple.second
                                 <| dealCards model.deck
                                 <| takeSetOut model.table switchedSelection
                         ,selection = init_selection
                         ,score = model.score + 1
                         }                         
                        , Cmd.none)
                    else
                        ({ model |
                               deck = model.deck
                               ,table =
                                   filteredBy (List.map not switchedSelection)
                                       <| model.table
                               ,selection =
                                   filteredBy (List.map not switchedSelection)
                                       <| switchedSelection
                               ,score = model.score + 1
                         }
                        , Cmd.none)
                else
                    ({ model |
                           selection = switchedSelection
                     }
                    ,Cmd.none)
            else
                (model, Cmd.none)
                    
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

        Resize n ->
            ({ model | size = n}, Cmd.none)
                
        Reset -> ({deck = []
                  ,table = init_table
                  ,selection = init_selection
                  ,score = 0
                  ,mode = Start
                  ,size = model.size
                  }
                 ,Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW


view : Model -> Html Msg
view model =
    let size = model.size in
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
                [ putCard size 0 model.table model.selection
                , putCard size 1 model.table model.selection
                , putCard size 2 model.table model.selection
                , putCard size 3 model.table model.selection
                , extraCard size 0 model.table model.selection
                , extraCard size 3 model.table model.selection
                ,  div [style [("background-color", "grey")
                    ,("width","80px")
                    ,("height","40px")
                    ,("display","inline-flex")
                    ,("position","relative")
                    ,("left","20px")
                    ,("align-items","center")
                    ,("justify-content","center")
                    ]] [text "¡Busca!"]
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
                , putCard size 4 model.table model.selection
                , putCard size 5 model.table model.selection
                , putCard size 6 model.table model.selection
                , putCard size 7 model.table model.selection
                , extraCard size 1 model.table model.selection
                , extraCard size 4 model.table model.selection
                , addMoreCards model.table model.deck
                , br [] []
                , putCard size 8 model.table model.selection
                , putCard size 9 model.table model.selection
                , putCard size 10 model.table model.selection
                , putCard size 11 model.table model.selection
                , extraCard size 2 model.table model.selection
                , extraCard size 5 model.table model.selection
                , br [] []
                , h3 [] [text "Tamaño"]
                , select []
                    [ option [onClick (Resize 140)] [text "pequeño"]
                    , option [onClick (Resize 170)] [text "normal"]
                    , option [onClick (Resize 200)] [text "grande"]
                    ]
                , button [onClick Reset] [text "Reset"]
                ]

