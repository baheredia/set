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

type alias Card = List Int

type alias Deck = List Card

type Mode
    = Start
    | Game
    
type alias Model =
    {deck : Deck
    ,table : Deck
    ,selection : List Bool
    ,score : Int
    ,mode : Mode
    }

init_deck : Deck
init_deck = triple4 [[]]

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

type Msg = Shuffle
    | PutDeck (List (List Int))
    | Select Natural
    | Set
    | Reset
    | ExtraCard
     
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
            if List.length (List.filter identity (togleElement n model.selection)) <= 3
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
                ({deck = Tuple.first (putMoreCards model.deck (dropSet model.table model.selection))
                 ,table = Tuple.second (putMoreCards model.deck (dropSet model.table model.selection))
                 ,selection = init_selection
                 ,score = model.score + 1
                 ,mode = model.mode
                 }                         
                , Cmd.none)
            else
                ({deck = model.deck
                 ,table = escoje (List.map not model.selection) model.table
                 ,selection = escoje (List.map not model.selection) model.selection
                 ,score = model.score + 1
                 ,mode = model.mode
                 }
                , Cmd.none)
        ExtraCard ->
            ({ model |
                   deck = Tuple.first (putMoreCards model.deck (model.table ++ [[],[],[]]))
                   ,table = Tuple.second (putMoreCards model.deck (model.table ++ [[],[],[]]))
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
                [h1 [] [text "Bienvenido a Set, ¿Listo para jugar?"]
                ,button [onClick Shuffle] [text "¡Empieza!"]
                ]
        Game ->
            div []
                [putCard 0 model, putCard 1 model, putCard 2 model, putCard 3 model
                ,extraCard 0 model
                ,extraCard 3 model
                ,haySet (isListSet (escoje model.selection model.table))
                ,div [style [("background-color", "pink")
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
                ,br [] []
                ,putCard 4 model, putCard 5 model, putCard 6 model, putCard 7 model
                ,extraCard 1 model
                ,extraCard 4 model
                ,addMoreCards model.table model.deck
                ,br [] []
                ,putCard 8 model, putCard 9 model, putCard 10 model, putCard 11 model
                ,extraCard 2 model
                ,extraCard 5 model
                ,button [onClick Reset] [text "Reset"]
                ]


putCard : Int -> Model -> Html Msg
putCard x model =
    let ancho = 170 in
    img [src (direccion (takeElementInPosition (ind x) model.table))
        , width ancho
        , style [("border", queBorde (takeElementInPosition (ind x) model.selection))]
        , onClick (Select (ind x))
        ] []

extraCard : Int -> Model -> Html Msg
extraCard n model =
    if List.length model.table > 12
    then putCard (12+n) model
    else span [] []

addMoreCards : List (List Int) -> Deck -> Html Msg
addMoreCards lst deck =
    let sty = style [("background-color", "yellow")
                    ,("cursor","pointer")
                    ,("width","100px")
                    ,("height","100px")
                    ,("display","inline-flex")
                    ,("position","relative")
                    ,("left","20px")
                    ,("align-items","center")
                    ,("justify-content","center")
                    ] in
    if (List.length lst > 15) || (List.isEmpty deck)
    then span [] []
    else div [onClick ExtraCard
             ,sty
             ] [text "Más cartas"]

      
haySet : Bool -> Html Msg
haySet b =
    let sty op1 op2 = style [("background-color", op1)
                            ,("cursor", op2)
                            ,("width","100px")
                            ,("height","100px")
                            ,("display","inline-flex")
                            ,("position","relative")
                            ,("left","20px")
                            ,("align-items","center")
                            ,("justify-content","center")
                            ] in
    case b of
        True -> div [onClick Set
                    , sty "red" "pointer"
                    ] [text "¡Set!"]
        False -> div [sty "grey" "default"
                     ] [text "¡Busca!"]
