-- In this module are all the functions used in the update
module Dynamics exposing (..)

import Generic exposing (..)

-- NOW FUNCTIONS USEFULL JUST FOR THE UPDATE
-- Time to add new cards

time_to_pass : Int  -- This you can change
time_to_pass = 20

init_table : Table
init_table = [Nothing, Nothing, Nothing, Nothing
             ,Nothing, Nothing, Nothing, Nothing
             ,Nothing, Nothing, Nothing, Nothing]

init_selection : List Bool
init_selection = [False,False,False,False
                 ,False,False,False,False
                 ,False,False,False,False
                 ]


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

-- Functions appearing in the UPDATE
-- This is how things change when you select a card
makeSelection : Int -> Model -> Model
makeSelection n model =
    let switchedSelection = switchElement n model.selection in
    if  -- you can still select a third card
        howManyTrue switchedSelection <= 3
    then
        if  -- you have selected a set
            isListSet <| filteredBy switchedSelection model.table
        then 
            if  -- you don't have extra cards in the table
                List.length model.table <= 12
            then 
                replaceSet model switchedSelection
            else --If there are extra cards
                takeOutSet model switchedSelection
        else -- if it is not a set just switch the selection
            { model | selection = switchedSelection }
    else
        model

-- This just counts the numbers of Trues (this is going to be used to know
-- how many selected cards are
howManyTrue : List Bool -> Int
howManyTrue = List.length << (List.filter identity)

-- This function replace a selected set in the table selected with new cards
replaceSet : Model -> List Bool -> Model
replaceSet model selectedPosition =
    let
        scoreToStop mode =
            case mode of
                Game -> 24
                OneColorGame -> 9
                _ -> 0
    in
        let
            newBestTime mode time best score =
                if score == (scoreToStop mode) - 1
                then
                    if (best > time) || (best == 0)
                    then time
                    else best
                else best
        in
            { model | deck =
                  Tuple.first
                      <| dealCards model.deck
                      <| takeSetOut model.table selectedPosition
            , table =
                Tuple.second
                    <| dealCards model.deck
                    <| takeSetOut model.table selectedPosition
            , selection = init_selection
            , score = model.score + 1
            , timeToAddCards = time_to_pass
            , bestTime =
                newBestTime model.mode model.time model.bestTime model.score
            }                         

-- This takes a set out of the table without replacing the cards
takeOutSet : Model -> List Bool -> Model
takeOutSet model selectedPosition =
    { model | deck = model.deck
    , table =
        filteredBy (List.map not selectedPosition)
            <| model.table
    , selection =
        filteredBy (List.map not selectedPosition)
            <| selectedPosition
    , score = model.score + 1
    , timeToAddCards = time_to_pass
    }


-- This updates the clock and all that
tick : Model -> Model
tick model =
    let scoreToStop mode =
            case mode of
                Game -> 24
                OneColorGame -> 9
                _ -> 0
    in
        if model.score >= scoreToStop model.mode
        then model
        else
            if  -- it is time to add cards, and there is space in the table,
                -- and cards in the deck
                model.timeToAddCards == 1
                && List.length model.table <= 15
                && not (List.isEmpty model.deck)
            then --add three extra cards
                addExtraCards model
            else -- Just make the time pass
                { model | time = model.time + 1
                , timeToAddCards = model.timeToAddCards - 1
                }

-- This puts three extra cards in the table
addExtraCards : Model -> Model
addExtraCards model =
    { model | deck =
                Tuple.first
                    <| dealCards model.deck
                    <| model.table ++ [Nothing,Nothing,Nothing]
    , table =
        Tuple.second
            <| dealCards model.deck
            <| model.table ++ [Nothing,Nothing,Nothing]
    , selection = model.selection ++ [False,False,False]
    , time = model.time + 1
    , timeToAddCards = time_to_pass
    }
