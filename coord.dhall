{- Boiler plate to read and show coords -}
let Prelude = ./Prelude.dhall

let Coord =
      < a
      | b
      | c
      | d
      | e
      | f
      | g
      | h
      | i
      | j
      | k
      | l
      | m
      | n
      | o
      | p
      | q
      | r
      | s
      | t
      | u
      | v
      | w
      | x
      | y
      | z
      >

let read
    : Natural -> Optional Coord
    = \(p : Natural) ->
        let eq = Prelude.Natural.equal

        in  if    eq p 0
            then  Some Coord.a
            else  if eq p 1
            then  Some Coord.b
            else  if eq p 2
            then  Some Coord.c
            else  if eq p 3
            then  Some Coord.d
            else  if eq p 4
            then  Some Coord.e
            else  if eq p 5
            then  Some Coord.f
            else  if eq p 6
            then  Some Coord.g
            else  if eq p 7
            then  Some Coord.h
            else  if eq p 8
            then  Some Coord.i
            else  if eq p 9
            then  Some Coord.j
            else  if eq p 10
            then  Some Coord.k
            else  if eq p 11
            then  Some Coord.l
            else  if eq p 12
            then  Some Coord.m
            else  if eq p 13
            then  Some Coord.n
            else  if eq p 14
            then  Some Coord.o
            else  if eq p 15
            then  Some Coord.p
            else  if eq p 16
            then  Some Coord.q
            else  if eq p 17
            then  Some Coord.r
            else  if eq p 18
            then  Some Coord.s
            else  if eq p 19
            then  Some Coord.t
            else  if eq p 20
            then  Some Coord.u
            else  if eq p 21
            then  Some Coord.v
            else  if eq p 22
            then  Some Coord.w
            else  if eq p 23
            then  Some Coord.x
            else  if eq p 24
            then  Some Coord.y
            else  if eq p 25
            then  Some Coord.z
            else  None Coord

let show
    : Coord -> Text
    = \(coord : Coord) ->
        merge
          { a = "a"
          , b = "b"
          , c = "c"
          , d = "d"
          , e = "e"
          , f = "f"
          , g = "g"
          , h = "h"
          , i = "i"
          , j = "j"
          , k = "k"
          , l = "l"
          , m = "m"
          , n = "n"
          , o = "o"
          , p = "p"
          , q = "q"
          , r = "r"
          , s = "s"
          , t = "t"
          , u = "u"
          , v = "v"
          , w = "w"
          , x = "x"
          , y = "y"
          , z = "z"
          }
          coord

in  { Type = Coord, read, show }
