exception TODO

type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2add (a: crazy2) (b: crazy2): crazy2 =
  raise TODO

