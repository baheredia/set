module Extras exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Types for the model

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

-- Types for messaging

type Msg = Shuffle
    | PutDeck Deck
    | Select Natural
    | Set
    | Reset
    | ExtraCard
     

-- This types are just because I like them
type Zero = Z

type alias Natural = List Zero

-- conversion from Int -> Natural
conversion : Int -> Natural
conversion n =
    if n <= 0
    then []
    else Z::(conversion (n-1))

-- to flatten Lists
flatten : List (List a) -> List a
flatten xs =
    case xs of
        [] -> []
        x::xs ->
          case x of
              [] -> flatten xs
              (y::ys) -> y :: (flatten (ys::xs))
-- this is to generate the Deck            
initialDeck : Deck
initialDeck =
    let newOption xs = [1::xs, 2::xs, 3::xs] in
    let
        tripleOnce xs = flatten
                        <| List.map newOption xs
    in 
    tripleOnce <| tripleOnce <| tripleOnce <| tripleOnce [[]]

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
                        (r::w4) ->
                            if (List.isEmpty w) || (List.isEmpty q) || (List.isEmpty r)
                            then False
                            else isSet w q r

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


howManyTrue : List Bool -> Int
howManyTrue = List.length << (List.filter identity)


putCard : Int -> Model -> Html Msg
putCard x model =
    let ancho = 170 in
    img [ src <| direccion
              <| takeElementInPosition (conversion x) model.table
        , width ancho
        , style [ ("border"
                  , queBorde
                       <| takeElementInPosition (conversion x) model.selection
                  )
                ]
        , onClick (Select (conversion x))
        ] []

extraCard : Int -> Model -> Html Msg
extraCard n model =
    if (List.length model.table > 15)
    then putCard (12+n) model
    else
        if (n <= 2) && (List.length model.table > 12)
        then putCard (12+n) model
        else span [] []

addMoreCards : Deck -> Deck -> Html Msg
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
              
