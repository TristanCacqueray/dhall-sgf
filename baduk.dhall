let Prelude = ./Prelude.dhall

let types = ./types.dhall

let Color/eq
    : types.Color -> types.Color -> Bool
    = \(c1 : types.Color) ->
      \(c2 : types.Color) ->
        merge
          { Black = merge { Black = True, White = False } c2
          , White = merge { Black = False, White = True } c2
          }
          c1

let Coord/read = (./coord.dhall).read

let Board/toPoints
    : types.Board -> types.Color -> List types.Point
    = \(board : types.Board) ->
      \(color : types.Color) ->
        let Row = List (Optional types.Color)

        let RowCoord = { index : Natural, value : List types.Coord }

        let {- First step converts each row to a List of Coord -}
            rowCoords
            : types.Board -> List (List types.Coord)
            = \(board : types.Board) ->
                let RowIndex = { index : Natural, value : Optional types.Color }

                let mkRowCoords
                    : List RowIndex -> List types.Coord
                    = let empty = Prelude.List.empty types.Coord

                      let foldRow =
                            \(cell : RowIndex) ->
                              merge
                                { None = empty
                                , Some =
                                    \(color : types.Color) ->
                                      if    Color/eq color color@1
                                      then  merge
                                              { None = empty
                                              , Some =
                                                  \(coord : types.Coord) ->
                                                    [ coord ]
                                              }
                                              (Coord/read cell.index)
                                      else  empty
                                }
                                cell.value

                      in  Prelude.List.concatMap RowIndex types.Coord foldRow

                let indexedRows =
                      Prelude.List.map
                        Row
                        (List RowIndex)
                        (Prelude.List.indexed (Optional types.Color))
                        board

                in  Prelude.List.map
                      (List RowIndex)
                      (List types.Coord)
                      mkRowCoords
                      indexedRows

        let indexedRowCoords
            : List RowCoord
            = Prelude.List.indexed (List types.Coord) (rowCoords board)

        let mkPoint
            : RowCoord -> List types.Point
            = \(row-coord : RowCoord) ->
                merge
                  { None = Prelude.List.empty types.Point
                  , Some =
                      \(y-coord : types.Coord) ->
                        Prelude.List.map
                          types.Coord
                          types.Point
                          ( \(x-coord : types.Coord) ->
                              { x = x-coord, y = y-coord }
                          )
                          row-coord.value
                  }
                  (Coord/read row-coord.index)

        in  Prelude.List.concatMap RowCoord types.Point mkPoint indexedRowCoords

let b = Some types.Color.Black

let w = Some types.Color.White

let o = None types.Color

let example-board =
      [ [ o, o, o, o, o ]
      , [ o, o, b, o, o ]
      , [ o, b, o, o, o ]
      , [ o, o, w, w, w ]
      , [ o, o, o, o, o ]
      ]

let example =
        assert
      :     Board/toPoints example-board types.Color.Black
        ===  [ { x = types.Coord.c, y = types.Coord.b }
             , { x = types.Coord.b, y = types.Coord.c }
             ]

let size-to-komi
    : Natural -> Double
    = \(size : Natural) -> if Prelude.Natural.equal size 9 then 3.5 else 0.0

let minimal
    : Natural -> types.GameTree
    = \(size : Natural) ->
        [ types.Property.Game types.GameType.Baduk
        , types.Property.Size size
        , types.Property.Komi (size-to-komi size)
        ]

let board
    : types.Board -> types.GameTree
    = \(board : types.Board) ->
        let moves = Board/toPoints board

        let size = Prelude.List.length (List (Optional types.Color)) board

        let black = moves types.Color.Black

        let white = moves types.Color.White

        let player =
              if    Prelude.Natural.greaterThan
                      (Prelude.List.length types.Point black)
                      (Prelude.List.length types.Point white)
              then  types.Color.White
              else  types.Color.Black

        in    minimal size
            # [ types.Property.BlackMoves black
              , types.Property.WhiteMoves white
              , types.Property.Player player
              ]

in  { minimal, board }
