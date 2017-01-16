module Extras exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Types for the model

type alias Card = (Int, Int, Int, Int)

type alias CardInTable = Maybe Card

type alias Deck = List Card

type alias Table = List CardInTable

type Mode
    = Start
    | Game
    
type alias Model =
    {deck : Deck
    ,table : Table
    ,selection : List Bool
    ,score : Int
    ,mode : Mode
    ,size : Int
    }

-- Types for messaging

type Msg = Shuffle Deck
    | PutDeck Deck
    | Select Natural
    | ExtraCard
    | Resize Int
    | Reset
      
-------------------------------------------------
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

--------------------------------------------
-- NOW FUNCTIONS USEFULL JUST FOR THE UPDATE

-- 1. Putting cards on the table
initialDeck : Bool -> Deck
initialDeck b =
    let
        produceOptions xs =
            let st e = [(1,e),(2,e),(3,e)] in
            flatten (List.map st xs)
    in
        let arrangeParenthesis (n1,(n2,(n3,n4))) = (n1,n2,n3,n4)
        in
            case b of
                True ->
                    let twist (n1,n2,n3,n4) = (n1,n2,n4,n3)
                    in
                        List.map (twist << arrangeParenthesis)
                            <| produceOptions
                            <| produceOptions
                            <| produceOptions [1]
                False ->
                    List.map arrangeParenthesis
                        <| produceOptions
                        <| produceOptions
                        <| produceOptions [1,2,3]



-- this is in normal circustances to take a set from the table
takeSetOut : Table -> List Bool -> Table
takeSetOut cards bs =
    case cards of
        [] -> []
        (x::xs) ->
            case bs of
                [] -> cards
                (b::bbs) ->
                    case b of
                        True -> Nothing :: (takeSetOut xs bbs)
                        False -> x::(takeSetOut xs bbs)

-- this is for dealing new cards to the table
dealCards : Deck -> Table -> (Deck, Table)
dealCards ondeck ontable =
    case ondeck of
        [] -> ([], ontable)
        (c::cs) ->
            case ontable of
                [] -> (ondeck, [])
                (t::ts) ->
                    case t of
                        Nothing ->
                            ( Tuple.first (dealCards cs ts)
                            , (Just c)::(Tuple.second (dealCards cs ts))
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
isSet : CardInTable -> CardInTable -> CardInTable -> Bool
isSet cardx cardy cardz =
    case cardx of
        Nothing -> False
        Just (x1,x2,x3,x4) ->
            case cardy of
                Nothing -> False
                Just (y1,y2,y3,y4) ->
                    case cardz of
                        Nothing -> False
                        Just (z1,z2,z3,z4) ->
                            (allEquals [x1,y1,z1] || allDifferent [x1,y1,z1])
                            && (allEquals [x2,y2,z2] || allDifferent [x2,y2,z2])
                            && (allEquals [x3,y3,z3] || allDifferent [x3,y3,z3])
                            && (allEquals [x4,y4,z4] || allDifferent [x4,y4,z4])

-- this just takes a deck and see if the three first cards forma a set,
isListSet : Table -> Bool
isListSet w1 =
    case w1 of
        [] -> False
        (w::w2) ->
            case w2 of
                [] -> False
                (q::w3) ->
                    case w3 of
                        [] -> False
                        (r::_) -> isSet w q r

-----------------------------------------------
-- This part is for displaying things on the web

-- This takes a Maybe Card and give back the file with its image
source : Maybe CardInTable -> String
source wd =
    case wd of
        Nothing -> ""
        Just w -> let toNombre xs =
                          case xs of
                              Nothing -> ""
                              Just (x1,x2,x3,x4) ->
                                  (toString x1)
                                  ++ (toString x2)
                                  ++ (toString x3)
                                  ++ (toString x4)
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
putCard : Int -> Int -> Table -> List Bool -> Html Msg
putCard size x table selection =
    let ancho = size in
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
extraCard : Int -> Int -> Table -> List Bool -> Html Msg
extraCard size n table selection =
    if (List.length table > 15)
    then putCard size (12+n) table selection
    else
        if (n <= 2) && (List.length table > 12)
        then putCard size (12+n) table selection
        else span [] []

-- This is the button to add cards
addMoreCards : Table -> Deck -> Html Msg
addMoreCards table deck =
    let sty op1 op2 =
            style [("background-color", op1)
                  ,("cursor",op2)
                  ,("width","80px")
                  ,("height","40px")
                  ,("display","inline-flex")
                  ,("position","relative")
                  ,("left","20px")
                  ,("align-items","center")
                  ,("justify-content","center")
                  ] in
    if (List.length table > 15) || (List.isEmpty deck)
    then div [sty "grey" "default"] [text "."]
    else div [onClick ExtraCard
             ,sty "yellow" "pointer"
             ] [text "Más"]


    
              
