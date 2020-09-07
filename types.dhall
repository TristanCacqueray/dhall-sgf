let GameType = < Baduk >

let Rule = < Japanese | Chinese >

let Color = < Black | White >

let Board = List (List (Optional Color))

let Coord = (./coord.dhall).Type

let Point = { x : Coord, y : Coord }

let Property =
      < Game : GameType
      | Size : Natural
      | GameName : Text
      | Date : Text
      | Komi : Double
      | Handicap : Natural
      | Rules : Rule
      | Application : Text
      | BlackMoves : List Point
      | WhiteMoves : List Point
      | Player : Color
      >

let GameTree = List Property

in  { GameType, Rule, Color, Board, Coord, Point, Property, GameTree }
