module Extras exposing (..)

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
