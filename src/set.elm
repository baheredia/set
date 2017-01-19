import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Random.List
import Random

import Extras exposing (..)
import Dynamics exposing (..)
import Generic exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

-- The initialization variables are in the Dynamics package
init : (Model, Cmd Msg)
init =  ({ deck = []
          --deck = List.drop 75 init_deck   --This is for testing
         , table = init_table
         , selection = init_selection
         , score = 0
         , mode = Start
         , size = 140
         , time = 0
         , timeToAddCards = time_to_pass
         , bestTime = 0
         }
        , Cmd.none)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Shuffle mode ->
            let start_model new_mode =
                    { model |
                        deck = []
                        , table = init_table
                        , selection = init_selection
                        , score = 0
                        , mode = new_mode
                        , time = 0
                        , timeToAddCards = 15
                    }
            in
                let oneColor game_mode =
                        case game_mode of
                            OneColorGame -> True
                            OneColorTraining -> True
                            _ -> False
                in
                    (start_model mode
                    , Random.generate PutDeck
                        <| Random.List.shuffle (initialDeck <| oneColor mode)
                    )

        PutDeck shuffled_deck ->
            ({ model |
                   deck = Tuple.first <| dealCards shuffled_deck model.table
                   ,table = Tuple.second <| dealCards shuffled_deck model.table
                   ,time = 0
                   ,timeToAddCards = time_to_pass
             }
            ,Cmd.none)
            
        Select n ->
            (makeSelection n model, Cmd.none)
                    
        ExtraCard ->
            (addExtraCards model, Cmd.none)

        Resize n ->
            ({ model | size = n}, Cmd.none)

        Tick _ ->
            (tick model, Cmd.none)
                
        Reset -> ({ model |
                        deck = []
                        ,table = init_table
                        ,selection = init_selection
                        ,score = 0
                        ,mode = Start
                        , time = 0
                        , timeToAddCards = 15
                  }
                 ,Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- VIEW


view : Model -> Html Msg
view model =
    case model.mode of
        Start ->
            initialPage
        Game ->
            gamePage model
        Training ->
            gamePage model
        OneColorTraining ->
            gamePage model
        OneColorGame ->
            gamePage model
