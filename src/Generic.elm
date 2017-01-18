-- In this module I will gather all the type definitions and generic functions

module Generic exposing (..)

import Time exposing (..)

-- Types for the model

type alias Card = (Int, Int, Int, Int)

type alias CardInTable = Maybe Card

type alias Deck = List Card

type alias Table = List CardInTable

type Mode
    = Start
    | Game
--    | Training
--    | OneColorTraining
--    | OneColorGame
    
type alias Model =
    {deck : Deck
    ,table : Table
    ,selection : List Bool
    ,score : Int
    ,mode : Mode
    ,size : Int
    ,time : Time
    ,timeToAddCards : Int
    ,timeAtStart : Time
    ,timeAt23 : Time
    }

-- Types for messaging

type Msg = Shuffle Deck
    | PutDeck Deck
    | Select Natural
--    | ExtraCard
    | Resize Int
    | Tick Time
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
