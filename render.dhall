let Prelude = ./Prelude.dhall

let types = ./types.dhall

let Property/texts
    : Text -> List Text -> Text
    = \(code : Text) ->
      \(values : List Text) ->
        let bracket-values =
              Prelude.Text.concatMap
                Text
                (\(value : Text) -> "[${value}]")
                values

        in  "${code}${bracket-values}"

let Property/text
    : Text -> Text -> Text
    = \(code : Text) -> \(value : Text) -> Property/texts code [ value ]

let Property/toText
    : Text -> forall (type : Type) -> (type -> Text) -> type -> Text
    = \(code : Text) ->
      \(type : Type) ->
      \(f : type -> Text) ->
      \(val : type) ->
        Property/texts code [ f val ]

let GameType/show
    : types.GameType -> Text
    = \(game-type : types.GameType) -> merge { Baduk = "1" } game-type

let Rule/show
    : types.Rule -> Text
    = \(rule : types.Rule) ->
        merge { Japanese = "Japanese", Chinese = "Chinese" } rule

let Color/show
    : types.Color -> Text
    = \(color : types.Color) -> merge { Black = "B", White = "W" } color

let Point/show
    : types.Point -> Text
    = \(point : types.Point) ->
        let Coord/show = (./coord.dhall).show

        in  Coord/show point.x ++ Coord/show point.y

let Moves
    : Text -> List types.Point -> Text
    = \(code : Text) ->
      \(points : List types.Point) ->
        Property/texts
          code
          (Prelude.List.map types.Point Text Point/show points)

let Property/show
    : types.Property -> Text
    = \(prop : types.Property) ->
        merge
          { Game = Property/toText "GM" types.GameType GameType/show
          , Size = Property/toText "SZ" Natural Natural/show
          , GameName = Property/text "GN"
          , Date = Property/text "DT"
          , Komi = Property/toText "KM" Double Double/show
          , Handicap = Property/toText "HA" Natural Natural/show
          , Rules = Property/toText "RU" types.Rule Rule/show
          , Application = Property/text "AP"
          , BlackMoves = Moves "AB"
          , WhiteMoves = Moves "AW"
          , Player = Property/toText "PL" types.Color Color/show
          }
          prop

let render
    : types.GameTree -> Text
    = \(game-tree : types.GameTree) ->
        let nodes =
              Prelude.Text.concatMap types.Property Property/show game-tree

        in  "(;${nodes})"

in  render
