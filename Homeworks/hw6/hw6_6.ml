exception TODO

type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val (c:crazy2): int =
  raise TODO

