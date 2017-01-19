-- In this module appear the functions for displaying the things on the web

module Extras exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Dynamics exposing (..)
import Generic exposing (..)



--------------------------------------------
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
          <| takeElementInPosition x table
        , width ancho
        , style [ ("border"
                  , whichBorder
                       <| takeElementInPosition x selection
                  )
                ]
        , onClick (Select x)
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
addMoreCards : Mode -> Table -> Deck -> Html Msg
addMoreCards mode table deck =
    let button = 
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
    in
        case mode of
            Start ->
                div [] []
            Game ->
                div [] []
            Training ->
                button
            OneColorGame ->
                div [] []
            OneColorTraining ->
                button

    
initialPage : Html Msg              
initialPage =
    div [style [("margin-left","5%")]]
        [ h1 [] [text "Bienvenido a Set, ¿Listo para jugar?"]
        , buttonsToStart Game
        , buttonsToStart OneColorGame
        , h2 [] [text "Para entrenar sin reloj y sin presiones"]
        , buttonsToStart Training
        , buttonsToStart OneColorTraining
        , h3 [] [text "Si no sabes jugar"]
        , button [] [text "Instrucciones"]
        , span [] [text "(aún no las he puesto)"]
        ]

buttonsToStart : Mode -> Html Msg
buttonsToStart mode =
    let
        sty op1 =
            style [("background-color", op1)
                  ,("display", "inline-flex")
                  ,("width", "200px")
                  ,("height","300px")
                  ,("align-items", "center")
                  ,("justify-content","center")
                  ,("cursor","pointer")
                  ,("margin-left", "2%")
                  ,("border-style", "outset")
                  ]
    in
        case mode of
            Start ->
                div [] []
            Game ->
                div [ sty "lightsteelblue"
                    , onClick <| Shuffle Game
                    ] [ text "Quiero jugar"
                      , br [] []
                      , text "con toda la baraja"
                      , br [] []
                      , text "(El reloj para"
                      , br [] []
                      , text "con 24 puntos)"]
                    
            Training ->
                div [ sty "peachpuff"
                    , onClick <| Shuffle Training
                    ] [text "Quiero entrenar"
                      ,br [] []
                      ,text "Con toda la baraja"
                      ]
            OneColorGame ->
                div [ sty "tomato"
                    , onClick <| Shuffle OneColorGame
                    ] [text "Quiero entrenar"
                      , br [] []
                      , text "con un único color"
                      , br [] []
                      , text "(El reloj para"
                      , br [] []
                      , text "con 9 puntos)"
                      ]
            OneColorTraining ->
                div [sty "lightpink"
                    , onClick <| Shuffle OneColorTraining
                    ] [text "Quiero entrenar"
                      , br [] []
                      , text "con un único color"
                      ]
                    

gamePage : Model -> Html Msg
gamePage model =
    let size = model.size in
    div [style [("margin-left","1%")
               ,("margin-top","1%")]]
        [ putCard size 0 model.table model.selection
        , putCard size 1 model.table model.selection
        , putCard size 2 model.table model.selection
        , putCard size 3 model.table model.selection
        , extraCard size 0 model.table model.selection
        , extraCard size 3 model.table model.selection
        , div [style [("background-color", "pink")
                     ,("display", "inline-flex")
                     ,("width","90px")
                     ,("height","40px")
                     ,("align-items","center")
                     ,("justify-content","center")
                     ,("position","relative")
                     ,("left","40px")
                     ]
              ]
              [text ("Puntos:\n " ++ toString model.score)]
        , clock model.mode model.time "blue"
        , br [] []
        , putCard size 4 model.table model.selection
        , putCard size 5 model.table model.selection
        , putCard size 6 model.table model.selection
        , putCard size 7 model.table model.selection
        , extraCard size 1 model.table model.selection
        , extraCard size 4 model.table model.selection
        , addMoreCards model.mode model.table model.deck
        , br [] []
        , putCard size 8 model.table model.selection
        , putCard size 9 model.table model.selection
        , putCard size 10 model.table model.selection
        , putCard size 11 model.table model.selection
        , extraCard size 2 model.table model.selection
        , extraCard size 5 model.table model.selection
        , br [] []
        , showBestTime model.mode model.bestTime
        , h3 [] [text "Tamaño"]
        , select []
            [ option [onClick (Resize 140)] [text "pequeño"]
            , option [onClick (Resize 170)] [text "normal"]
            , option [onClick (Resize 200)] [text "grande"]
            ]
        , button [onClick <| Shuffle model.mode]
            [text "Barajar de nuevo"]
        , button [onClick Reset] [text "Volver"]
        ]

showBestTime : Mode -> Int -> Html Msg
showBestTime mode best =
    case mode of
        Game -> div [] [ text "Mejor tiempo:"
                       , clock mode best "olive"]
        OneColorGame -> div [] [ text "Mejor tiempo:"
                               , clock mode best "olive"]
        _ -> div [] []
        
clock : Mode -> Int -> String -> Html Msg
clock mode time color =
    let sty = style [("background-color", color)
                    ,("display", "inline-flex")
                    ,("width","80px")
                    ,("height","40px")
                    ,("align-items","center")
                    ,("justify-content","center")
                    ,("position","relative")
                    ,("left","40px")
                    ,("color","white")
                     ]
    in
        case mode of
            Start ->
                div [] []
            Game ->
                div [sty] [text (toString time)]
            Training ->
                div [] []
            OneColorGame ->
                div [sty] [text (toString time)]
            OneColorTraining ->
                div [] []

