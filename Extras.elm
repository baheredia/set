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
     
-- THIS PARTS IS ABOUT GENERIC FUNCTIONS
-- This types are just because I like them
type One = Z

type alias Natural = List One

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

-- this is for switching an element in a list of booelans               
switchElement : Natural -> List Bool -> List Bool
switchElement n bs =
    case bs of
        [] -> []
        (x::xs) ->
            case n of
                [] -> (not x)::xs
                (z::zs) -> x::(switchElement zs xs)

-- This filters a list from another list of booleans
filteredBy : List Bool -> List a -> List a
filteredBy bls xs =
    case bls of
        [] -> []
        (b::bs) ->
            case xs of
                [] -> []
                (y::ys) -> if b
                           then (y::(filteredBy bs ys))
                           else filteredBy bs ys

-- This is takes the element in the position indicated from a list if
-- it exists
takeElementInPosition : Natural -> List a -> Maybe a
takeElementInPosition n xs =
    case n of
        [] -> case xs of
                 [] -> Nothing
                 (x::xs) -> Just x
        (n::ns) -> case xs of
                     [] -> Nothing
                     (x::xs) -> takeElementInPosition ns xs

-- This just counts the numbers of Trues (this is going to be used to know
-- how many selected cards are
howManyTrue : List Bool -> Int
howManyTrue = List.length << (List.filter identity)

-- NOW FUNCTIONS USEFULL JUST FOR THE UPDATE

-- 1. Putting cards on the table
-- this is to generate the Deck            
initialDeck : Deck
initialDeck =
    let newOption xs = [1::xs, 2::xs, 3::xs] in
    let
        tripleOnce xs = flatten
                        <| List.map newOption xs
    in 
    tripleOnce <| tripleOnce <| tripleOnce <| tripleOnce [[]]

-- this is in normal circustances to take a set from the table
takeSetOut : Deck -> List Bool -> Deck
takeSetOut cards bs =
    case cards of
        [] -> []
        (x::xs) ->
            case bs of
                [] -> cards
                (b::bbs) ->
                    case b of
                        True -> []::(takeSetOut xs bbs)
                        False -> x::(takeSetOut xs bbs)

-- this is for dealing new cards to the table
dealCards : Deck -> Deck -> (Deck, Deck)
dealCards ondeck ontable =
    case ondeck of
        [] -> ([], ontable)
        (c::cs) ->
            case ontable of
                [] -> (ondeck, [])
                (t::ts) ->
                    case t of
                        [] -> (Tuple.first (dealCards cs ts)
                              ,c::(Tuple.second (dealCards cs ts))
                              )
                        _ -> (Tuple.first (dealCards ondeck ts)
                             ,t::(Tuple.second (dealCards ondeck ts))
                             )
                           
-- 2. To see if it is a set
-- this checks if all the elements in a list are equal
allEquals : List a -> Bool
allEquals xs =
    case xs of
        [] -> True
        (y::ys) ->
            case ys of
                [] -> True
                (z::zs) -> if y == z
                           then allEquals ys
                           else False

-- this checks if all element in a list are different
allDifferent : List a -> Bool
allDifferent xs =
    case xs of
        [] -> True
        (y::ys) ->
            case ys of
                [] -> True
                (z::zs) -> if y == z
                           then False
                           else (allDifferent (y::zs)) && (allDifferent (z::zs))

-- this takes three cards and says if it is a set
isSet : Card -> Card -> Card -> Bool
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
                            if
                                (allEquals [y1,y2,y3]
                                || allDifferent [y1,y2,y3])
                            then isSet y1s y2s y3s
                            else False

-- this just takes a deck and see if the three first cards forma a set,
-- with this you avoid the case where some cards are empty (it was a problem)
isListSet : Deck -> Bool
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
                            if
                                (List.isEmpty w)
                                || (List.isEmpty q)
                                || (List.isEmpty r)
                            then False
                            else isSet w q r

-- This part is for displaying things on the web

-- This takes a Maybe Card and give back the file with its image
source : Maybe Card -> String
source wd =
    case wd of
        Nothing -> ""
        Just w -> let toNombre xs =
                          case xs of
                              [] -> ""
                              (x::xs) -> (toString x) ++ (toNombre xs)
                  in
                      "img/c" ++ (toNombre w) ++ ".png"
      
-- This tells if something is selected and so if it should have a border
whichBorder : Maybe Bool -> String
whichBorder b =
    case b of
        Nothing -> "none"
        Just b2 -> case b2 of
                       True -> "dashed"
                       False -> "solid white"


-- This takes a position from 0 .. 17 and puts a displays a card in that
-- position (with the border, that is why it needs the list of selected cards)
putCard : Int -> Deck -> List Bool -> Html Msg
putCard x table selection =
    let ancho = 170 in
    img [ src
          <| source
          <| takeElementInPosition (conversion x) table
        , width ancho
        , style [ ("border"
                  , whichBorder
                       <| takeElementInPosition (conversion x) selection
                  )
                ]
        , onClick (Select (conversion x))
        ] []

-- This is for the cases in which you have extra cards
extraCard : Int -> Deck -> List Bool -> Html Msg
extraCard n table selection =
    if (List.length table > 15)
    then putCard (12+n) table selection
    else
        if (n <= 2) && (List.length table > 12)
        then putCard (12+n) table selection
        else span [] []

-- This is the button to add cards
addMoreCards : Deck -> Deck -> Html Msg
addMoreCards lst deck =
    let sty op1 op2 =
            style [("background-color", op1)
                  ,("cursor",op2)
                  ,("width","100px")
                  ,("height","100px")
                  ,("display","inline-flex")
                  ,("position","relative")
                  ,("left","20px")
                  ,("align-items","center")
                  ,("justify-content","center")
                  ] in
    if (List.length lst > 15) || (List.isEmpty deck)
    then div [sty "grey" "default"] [text "."]
    else div [onClick ExtraCard
             ,sty "yellow" "pointer"
             ] [text "Más cartas"]

-- This is for the Set button
setButton : Bool -> Html Msg
setButton b =
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
              
