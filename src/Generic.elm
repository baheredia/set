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
    | Training
    | OneColorTraining
    | OneColorGame
    | Instructions
    
type alias Model =
    {deck : Deck
    ,table : Table
    ,selection : List Bool
    ,score : Int
    ,mode : Mode
    ,size : Int
    ,time : Int
    ,timeToAddCards : Int
    ,bestTime : Int
    }

-- Types for messaging

type Msg = Shuffle Mode
    | PutDeck Deck
    | Select Int
    | ExtraCard
    | Resize Int
    | Tick Time
    | Help
    | Reset
      
-------------------------------------------------
-- THIS PARTS IS ABOUT GENERIC FUNCTIONS

-- to flatten Lists
flatten : List (List a) -> List a
flatten xs =
    case xs of
        [] -> []
        x::xs ->
          case x of
              [] -> flatten xs
              (y::ys) -> y :: (flatten (ys::xs))

-- This switch the in position n of a boolean list
switchElement : Int -> List Bool -> List Bool
switchElement n bs =
    let end = List.drop n bs in
    let listMaybe l =
            case l of
                Nothing -> []
                Just a -> a
    in
        let
            notMaybe m =
                case m of
                    Nothing -> False
                    Just b -> not b
        in
            List.take n bs
            ++ (notMaybe <| List.head end) :: (listMaybe <| List.tail end)


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

takeElementInPosition : Int -> List a -> Maybe a
takeElementInPosition n xs =
    List.head <| List.drop n xs
