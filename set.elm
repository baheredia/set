import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random.List
import Random

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
    {deck : List (List Int)
    ,table : List (List Int)
    ,seleccion : List Bool
    ,cuantos : Int               
    ,puntuacion : Int
    ,inicio : Bool
    }

type Zero = Z

type alias Natural = List Zero

ind : Int -> Natural
ind n =
    if n <= 0
    then []
    else Z::(ind (n-1))

flatt : List (List a) -> List a
flatt xs =
    case xs of
        [] -> []
        x::xs ->
          case x of
              [] -> flatt xs
              (y::ys) -> y :: (flatt (ys::xs))
                 
triple4 : List (List Int) -> List (List Int)
triple4 =
    let newOption xs = [1::xs, 2::xs, 3::xs] in
    let tripleOnce xs = flatt (List.map newOption xs) in 
    tripleOnce << tripleOnce << tripleOnce << tripleOnce


init_deck : List (List Int)
init_deck = triple4 [[]]

init_table : List (List Int)
init_table = [[],[],[],[],[],[],[],[],[],[],[],[]]

init_seleccion : List Bool
init_seleccion = [False,False,False,False
                 ,False,False,False,False
                 ,False,False,False,False
                 ]

init : (Model, Cmd Msg)
init =  ({deck = init_deck
         ,table = init_table
         ,seleccion = init_seleccion
         ,cuantos = 0
         ,puntuacion = 0
         ,inicio = True
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
            ({deck = model.deck
             ,table = model.table
             ,seleccion = model.seleccion
             ,cuantos = model.cuantos
             ,puntuacion = model.puntuacion
             ,inicio = False}
            ,Random.generate PutDeck (Random.List.shuffle model.deck))
        PutDeck list ->
            ({deck = List.drop 12 list
             ,table = List.take 12 list
             ,seleccion = model.seleccion
             ,cuantos = model.cuantos
             ,puntuacion = model.puntuacion
             ,inicio = False
             }
            ,Cmd.none)
        Select n ->
            if List.length (List.filter identity (togleElement n model.seleccion)) <= 3
            then
                ({deck = model.deck
                 ,table = model.table
                 ,seleccion = (togleElement n model.seleccion)
                 ,cuantos = List.length (List.filter identity (togleElement n model.seleccion))
                 ,puntuacion = model.puntuacion
                 ,inicio = False
                 }
                ,Cmd.none)
            else
                (model, Cmd.none)

        Reset -> ({deck = init_deck
                  ,table = init_table
                  ,seleccion = init_seleccion
                  ,cuantos = 0
                  ,puntuacion = 0
                  ,inicio = True
                  }
                 ,Cmd.none)
        Set ->
            if faltanCartas (escoje model.seleccion model.table)
            then
                (model, Cmd.none)
            else
                if List.length model.table <= 12
                then
                    ({deck = Tuple.first (putMoreCards model.deck (dropSet model.table model.seleccion))
                     ,table = Tuple.second (putMoreCards model.deck (dropSet model.table model.seleccion))
                     ,seleccion = init_seleccion
                     ,cuantos = 0
                     ,puntuacion = model.puntuacion + 1
                     ,inicio = False
                     }                         
                    , Cmd.none)
                else
                    ({deck = model.deck
                     ,table = escoje (List.map not model.seleccion) model.table
                     ,seleccion = escoje (List.map not model.seleccion) model.seleccion
                     ,cuantos = 0
                     ,puntuacion = model.puntuacion + 1
                     ,inicio = False
                     }
                    , Cmd.none)
        ExtraCard ->
            ({deck = Tuple.first (putMoreCards model.deck (model.table ++ [[],[],[]]))
             ,table = Tuple.second (putMoreCards model.deck (model.table ++ [[],[],[]]))
             ,seleccion = model.seleccion ++ [False,False,False]
             ,cuantos = model.cuantos
             ,puntuacion = model.puntuacion
             ,inicio = False
             }
            , Cmd.none
            )

faltanCartas : List (List Int) -> Bool
faltanCartas ls =
    case ls of
        [] -> False
        (x::xs) -> case x of
                       [] -> True
                       _ -> faltanCartas xs

dropSet : List (List Int) -> List Bool -> List (List Int)
dropSet cards bs =
    case cards of
        [] -> []
        (x::xs) ->
            case bs of
                [] -> cards
                (b::bbs) ->
                    case b of
                        True -> []::(dropSet xs bbs)
                        False -> x::(dropSet xs bbs)

putMoreCards : List (List Int) -> List (List Int) -> (List (List Int), List (List Int))
putMoreCards ondeck ontable =
    case ondeck of
        [] -> ([], ontable)
        (c::cs) ->
            case ontable of
                [] -> (ondeck, [])
                (t::ts) ->
                    case t of
                        [] -> (Tuple.first (putMoreCards cs ts)
                              ,c::(Tuple.second (putMoreCards cs ts))
                              )
                        _ -> (Tuple.first (putMoreCards ondeck ts)
                             ,t::(Tuple.second (putMoreCards ondeck ts))
                             )
                           
               
togleElement : Natural -> List Bool -> List Bool
togleElement n bs =
    case bs of
        [] -> []
        (x::xs) ->
            case n of
                [] -> (not x)::xs
                (z::zs) -> x::(togleElement zs xs)

allEquals : List Int -> Bool
allEquals xs =
    case xs of
        [] -> True
        (y::ys) ->
            case ys of
                [] -> True
                (z::zs) -> if y == z
                           then allEquals ys
                           else False

allDifferent : List Int -> Bool
allDifferent xs =
    case xs of
        [] -> True
        (y::ys) ->
            case ys of
                [] -> True
                (z::zs) -> if y == z
                           then False
                           else (allDifferent (y::zs)) && (allDifferent (z::zs))

isSet : List Int -> List Int -> List Int -> Bool
isSet x1s x2s x3s =
    case x1s of
        [] -> True
        (y1::y1s) ->
            case x2s of
                [] -> False
                (y2::y2s) ->
                    case x3s of
                        [] -> False
                        (y3::y3s) ->
                            if (allEquals [y1,y2,y3] || allDifferent [y1,y2,y3])
                            then isSet y1s y2s y3s
                            else False

isListSet : List (List Int) -> Bool
isListSet w1 =
    case w1 of
        [] -> False
        (w::w2) ->
            case w2 of
                [] -> False
                (q::w3) ->
                    case w3 of
                        [] -> False
                        (r::w4) -> isSet w q r

escoje : List Bool -> List a -> List a
escoje bls xs =
    case bls of
        [] -> []
        (b::bs) ->
            case xs of
                [] -> []
                (y::ys) -> if b
                           then (y::(escoje bs ys))
                           else escoje bs ys

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW


view : Model -> Html Msg
view model =
    case model.inicio of
        True ->
            div []
                [h1 [] [text "Bienvenido a Set, ¿Listo para jugar?"]
                ,button [onClick Shuffle] [text "¡Empieza!"]
                ]
        False ->
            div []
                [putCard 0 model, putCard 1 model, putCard 2 model, putCard 3 model
                ,extraCard 0 model
                ,extraCard 3 model
                ,haySet (isListSet (escoje model.seleccion model.table))
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
                     [text ("Llevas " ++ toString model.puntuacion ++ " sets")]
                ,br [] []
                ,putCard 4 model, putCard 5 model, putCard 6 model, putCard 7 model
                ,extraCard 1 model
                ,extraCard 4 model
                ,addMoreCards model.table
                ,br [] []
                ,putCard 8 model, putCard 9 model, putCard 10 model, putCard 11 model
                ,extraCard 2 model
                ,extraCard 5 model
                ,button [onClick Reset] [text "Reset"]
                ]

addMoreCards : List (List Int) -> Html Msg
addMoreCards lst =
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
    if List.length lst > 15
    then span [] []
    else div [onClick ExtraCard
             ,sty
             ] [text "Más cartas"]

extraCard : Int -> Model -> Html Msg
extraCard n model =
    if List.length model.table > 12
    then putCard (12+n) model
    else span [] []
      
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
      
direccion : Maybe (List Int)-> String
direccion wd =
    case wd of
        Nothing -> ""
        Just w -> let toNombre xs =
                          case xs of
                              [] -> ""
                              (x::xs) -> (toString x) ++ (toNombre xs)
                  in
                      "img/c" ++ (toNombre w) ++ ".png"
      
putCard : Int -> Model -> Html Msg
putCard x model =
    let ancho = 170 in
    img [src (direccion (takeElementInPosition (ind x) model.table))
        , width ancho
        , style [("border", queBorde (takeElementInPosition (ind x) model.seleccion))]
        , onClick (Select (ind x))
        ] []

queBorde : Maybe Bool -> String
queBorde b =
    case b of
        Nothing -> "hidden"
        Just b2 -> case b2 of
                       True -> "dashed"
                       False -> "hidden"

takeElementInPosition : Natural -> List a -> Maybe a
takeElementInPosition n xs =
    case n of
        [] -> case xs of
                 [] -> Nothing
                 (x::xs) -> Just x
        (n::ns) -> case xs of
                     [] -> Nothing
                     (x::xs) -> takeElementInPosition ns xs

