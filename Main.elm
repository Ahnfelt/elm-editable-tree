import Html.App exposing (beginnerProgram)
import List
import Html exposing (..)
import Html.Events exposing (onClick)


main =
  beginnerProgram { 
    model = Model {children = []}, 
    view = view identity, 
    update = update 
  }


type Model = Model { children : List Model }

type Msg 
  = At Int Msg
  | AddChild
  | RemoveSelf

update msg (Model model) =
  case msg of
    At i msg ->
      case split i model.children of
        (_, Nothing, _) -> Model model
        (front, Just middle, back) ->
          let result = if msg == RemoveSelf then [] else [ update msg middle ] in  
          Model { model | children = front ++ result ++ back }
    AddChild -> 
      Model { model | children = model.children ++ [ Model { children = [] } ] }
    RemoveSelf -> 
      Model { model | children = [] }

view at (Model model) =
  div [] [ 
    text ("Node "),
    button [onClick (at AddChild)] [text "+"],
    button [onClick (at RemoveSelf)] [text "-"],
    ul [] (List.indexedMap (childView at) model.children) 
  ]

childView at i child =
  li [] [view (\m -> at (At i m)) child]



{- split 2 [a, b, c, d, e] = ([a, b], Just c, [d, e]) -} 
split i list =
  let rest = List.drop i list in
  (
    List.take i list,
    List.head rest,
    List.drop 1 rest
  )
