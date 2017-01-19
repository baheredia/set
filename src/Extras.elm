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
            Training ->
                button
            OneColorTraining ->
                button
            _ ->
                div [] []

    
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
        , button [onClick Help] [text "Instrucciones"]
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
            _ ->
                div [] []
                    

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
            Game ->
                div [sty] [text (toString time)]
            OneColorGame ->
                div [sty] [text (toString time)]
            _ ->
                div [] []

instructionPage : Html Msg
instructionPage =
    let size = 120 in
    div [style [("margin-left","5%")
               , ("margin-top","2%")
               , ("width", "800px")
               ]
        ]
        [h1 [] [text "Cómo jugar a set"]
        , p []
            [text ("En el juego, habrá doce cartas sobre la mesa, "
                   ++ "cada una de estas cartas tiene cuatro atributos: "
                   ++ "color, número, relleno y forma.")
            , ul []
                [ li [] [text "Color: Rojo, Azul o Verde"]
                , li [] [text "Número: Uno, Dos o Tres"]
                , li [] [text "Relleno: Vacío, Rallado o Lleno"]
                , li [] [text "Forma: Círculo, Rombo o Gusano"]
                ]
            ]
        , p []
            [ img [ style [("float","left")]
                  , src "img/c1111.png"
                  , width size] []
            , br [] []
            , div [] [text "Color: Rojo"]
            , br [] []
            , div [] [text "Número: Uno"]
            , br [] []
            , div [] [text "Relleno: Blanco"]
            , br [] []
            , div [] [text "Forma: Rombo"]
            ]
        , br [] []
        , p []
            [text ("El objetivo del juego es encontrar tres cartas que "
                   ++ "para cada atributo sean todas iguales o todas "
                   ++ "diferentes, es decir verifican las cuatro condiciones:")
            , ul []
                [li [] [text ("Todas tienen el mismo color o todas tienen un "
                              ++ "color diferente")]
                ,li [] [text ("Todas tienen el mismo número o todas tienen "
                              ++ "un numero diferente")]
                , li [] [text ("Todas tienen el mismo relleno o todas tienen "
                               ++ "un relleno diferente")]
                , li [] [text ("Todas tienen la misma forma o todas tienen "
                               ++ "una forma diferente")]
                ]
            , text "Veamos algunos ejemplos:"
            ]
        , p []
            [ img [ style [("float","left")]
                  , src "img/c1321.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c2311.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c3331.png"
                  , width size] []
            , br [] []
            , div [] [text "Todas tienen un color diferente"]
            , br [] []
            , div [] [text "Todas tienen un número diferente"]
            , br [] []
            , div [] [text "Todas tienen el mismo relleno"]
            , br [] []
            , div [] [text "Todas tienen la misma forma"]
            ]
        , br [] []
        , p []
            [ img [ style [("float","left")]
                  , src "img/c3221.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c2322.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c1123.png"
                  , width size] []
            , br [] []
            , div [] [text "Todas tienen el mismo color"]
            , br [] []
            , div [] [text "Todas tienen un número diferente"]
            , br [] []
            , div [] [text "Todas tienen un relleno diferente"]
            , br [] []
            , div [] [text "Todas tienen una forma diferente"]
            ]
        , br [] []
        , p []
            [ img [ style [("float","left")]
                  , src "img/c1311.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c2123.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c3232.png"
                  , width size] []
            , br [] []
            , div [] [text "Todas tienen un color diferente"]
            , br [] []
            , div [] [text "Todas tienen un número diferente"]
            , br [] []
            , div [] [text "Todas tienen un relleno diferente"]
            , br [] []
            , div [] [text "Todas tienen una forma diferente"]
            ]
        , br [] []
        , p [] [text "Los siguientes no forman sets"]
        , p []
            [ img [ style [("float","left")]
                  , src "img/c1211.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c1222.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c1223.png"
                  , width size] []
            , br [] []
            , div [style [("color","red")]]
                [text "Falla el color"]
            , br [] []
            , div [] [text "Todas tienen el mismo número"]
            , br [] []
            , div [] [text "Todas tienen el mismo relleno"]
            , br [] []
            , div [] [text "Todas tienen una forma diferente"]
            ]
        , br [] [] 
        , p []
            [ img [ style [("float","left")]
                  , src "img/c2221.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c3312.png"
                  , width size] []
            , img [ style [("float","left")]
                  , src "img/c3133.png"
                  , width size] []
            , br [] []
            , div [] [text "Todas tienen un color diferente"]
            , br [] []
            , div [ style [("color","red")]]
                [text "Falla el número"]
            , br [] []
            , div [] [text "Todas tienen un relleno diferente"]
            , br [] []
            , div [] [text "Todas tienen una forma diferente"]
            ]
        , br [] []
        , p []
            [text ("Hay dos modalidades de juego: contrarreloj "
                   ++ "en la que tienes que conseguir 24 sets en "
                   ++ "el menor tiempo posible, y entrenamiento "
                   ++ "donde puedes buscar sets a tu ritmo sin prisas.")]
        , p []
            [text ("En modo contrarreloj si pasas mucho tiempo sin "
                   ++ "seleccionar un set se añadirán tres cartas "
                   ++ "automáticamente, hasta un máximo de 18. En "
                   ++ "modo entrenamiento hay un botón para echar más cartas.")
            ]
        , p []
            [text ("Si no ves ningún set más y no salen más cartas, mala "
                   ++ "suerte. Tendrás que empezar de nuevo.")
            ]
        , p []
            [text ("También puedes jugar con una baraja de un único color "
                  ++ "para practicar si estás empezando. En modo contrarreloj "
                  ++ "hay que conseguir 9 sets, pero cuidado porque no siempre "
                  ++ "es posible")
            ]
        , p [] [ button [onClick Reset] [text "Volver"]
                 ]
        ]
